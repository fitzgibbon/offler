/* Native backing for offler: SDL3 for the window and input, wgpu-native for
 * the drawing, stb_image for texture decoding.
 *
 * The granularity here deliberately matches the JavaScript lambdas in
 * Offler/Web/Gpu.idr, one C function per `%foreign`. That is not a
 * coincidence: Idris cannot build nested C structs across the FFI, so
 * descriptors have to be assembled on this side -- which is exactly what a
 * JS lambda was already doing. No scene logic and no layout constants live
 * here: sizes, strides, bind group entries and vertex attributes all arrive
 * as spec strings from Offler.Gfx.Layout, where they are derived and proved.
 */
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include <SDL3/SDL.h>
#include <webgpu/webgpu.h>

#define STB_IMAGE_IMPLEMENTATION
#define STBI_ONLY_PNG
#define STBI_ONLY_JPEG
#define STBI_ONLY_BMP
#define STBI_ONLY_GIF
#define STBI_ONLY_TGA
#include <stb_image.h>

#ifdef __APPLE__
#include <SDL3/SDL_metal.h>
#endif

#define MAX_BINDINGS 16
#define MAX_ATTRS    8
#define MAX_TEX_SLOTS 4

#define SV(s) ((WGPUStringView){ .data = (s), .length = strlen(s) })

/* ------------------------------------------------------------------ arrays */

float *offler_f32_new(int n) { return (float *)calloc((size_t)n, sizeof(float)); }
void offler_f32_poke(float *a, int i, double v) { a[i] = (float)v; }
double offler_f32_peek(float *a, int i) { return (double)a[i]; }

void offler_f32_poke4(float *a, int o, double x, double y, double z, double w) {
  a[o] = (float)x; a[o+1] = (float)y; a[o+2] = (float)z; a[o+3] = (float)w;
}

void offler_f32_poke16(float *a, int o,
                       double x0, double x1, double x2, double x3,
                       double x4, double x5, double x6, double x7,
                       double x8, double x9, double x10, double x11,
                       double x12, double x13, double x14, double x15) {
  a[o+0]=(float)x0;  a[o+1]=(float)x1;  a[o+2]=(float)x2;  a[o+3]=(float)x3;
  a[o+4]=(float)x4;  a[o+5]=(float)x5;  a[o+6]=(float)x6;  a[o+7]=(float)x7;
  a[o+8]=(float)x8;  a[o+9]=(float)x9;  a[o+10]=(float)x10; a[o+11]=(float)x11;
  a[o+12]=(float)x12; a[o+13]=(float)x13; a[o+14]=(float)x14; a[o+15]=(float)x15;
}

unsigned *offler_u32_new(int n) { return (unsigned *)calloc((size_t)n, sizeof(unsigned)); }
void offler_u32_poke(unsigned *a, int i, int v) { a[i] = (unsigned)v; }

/* ------------------------------------------------------------------- types */

typedef struct {
  WGPUBuffer buf;
  int count;
  int topo;              /* 0 triangles, 1 lines */
  WGPUBuffer ibuf;       /* index buffer, or NULL */
  int icount;
} Mesh;

typedef struct {
  int tex[MAX_TEX_SLOTS];
  WGPUBindGroup bg;
} BGEntry;

typedef struct {
  WGPURenderPipeline triO, triB, lineO, lineB;
  WGPUBindGroupLayout bgl;
  int texCount;
  int matSize;
  BGEntry *bgs;
  int bgCount, bgCap;
} Mat;

typedef struct {
  int asset, mesh, slot;
  double depth;
} Pending;

typedef struct {
  SDL_Window *window;
  WGPUInstance instance;
  WGPUAdapter adapter;
  WGPUDevice device;
  WGPUQueue queue;
  WGPUSurface surface;
  WGPUTextureFormat format;
  WGPUBuffer globalBuf, objBuf, matBuf, lineBuf;
  WGPUTexture depth;
  WGPUTextureView depthView;
  WGPUSampler sampler;
  WGPUTextureView white;
  WGPURenderPipeline linePipeline;
  WGPUBindGroup lineBindGroup;
  int width, height;
  int lineCount;

  /* Told to us by Idris, from Offler.Gfx.Layout -- not defined twice. */
  int globalSize, objSize, objStride, maxObjects;
  int meshStride, lineStride;
  WGPUVertexAttribute meshAttrs[MAX_ATTRS];
  int meshAttrCount;
  WGPUVertexAttribute lineAttrs[MAX_ATTRS];
  int lineAttrCount;

  /* Material assets: a slot of the material buffer, textures, and the
   * cached bind group made when the asset was. */
  struct Asset { int mat, slot; int tex[MAX_TEX_SLOTS]; WGPUBindGroup bg; } *assets;
  int assetCount, assetCap;

  Mesh *meshes;
  int meshCount, meshCap;
  WGPUTextureView *texs;
  int texCount, texCap;
  Mat *mats;
  int matCount, matCap;
  Pending *pend;
  int pendCount, pendCap;

  /* Live only between offler_begin and offler_end. */
  WGPUCommandEncoder encoder;
  WGPURenderPassEncoder pass;
  WGPUTexture frameTex;
  WGPUTextureView frameView;
  WGPUBuffer boundVerts;
  WGPURenderPipeline boundPipe;

  /* The event being reported, refreshed by each offler_poll. */
  char eventKey[32];
  double eventX, eventY, eventWheel;
  int eventButton;
} Ctx;

/* ------------------------------------------------------------------ helpers */

/* Records semicolon-separated, fields comma-separated, variable length. */
static const char *spec_int(const char *s, int *out) {
  *out = (int)strtol(s, (char **)&s, 10);
  if (*s == ',') s++;
  return s;
}

static WGPUVertexFormat f32_format(int n) {
  switch (n) {
    case 1:  return WGPUVertexFormat_Float32;
    case 2:  return WGPUVertexFormat_Float32x2;
    case 4:  return WGPUVertexFormat_Float32x4;
    default: return WGPUVertexFormat_Float32x3;
  }
}

/* Parse a kinded bind spec into layout entries.
 *   0,slot,visibility,dynamic,minBindingSize   uniform buffer
 *   1,slot                                     texture (fragment)
 *   2,slot                                     its sampler                */
static int parse_bind_spec(const char *s, WGPUBindGroupLayoutEntry *out,
                           int *matSize, int *texCount) {
  int n = 0;
  *texCount = 0;
  while (*s && n < MAX_BINDINGS) {
    int kind, slot;
    s = spec_int(s, &kind);
    s = spec_int(s, &slot);
    memset(&out[n], 0, sizeof(out[n]));
    out[n].binding = (uint32_t)slot;
    if (kind == 0) {
      int vis, dyn, size;
      s = spec_int(s, &vis);
      s = spec_int(s, &dyn);
      s = spec_int(s, &size);
      out[n].visibility = (WGPUShaderStage)vis;
      out[n].buffer.type = WGPUBufferBindingType_Uniform;
      out[n].buffer.hasDynamicOffset = dyn != 0;
      out[n].buffer.minBindingSize = (uint64_t)size;
      if (slot == 2 && matSize) *matSize = size;
    } else if (kind == 1) {
      out[n].visibility = WGPUShaderStage_Fragment;
      out[n].texture.sampleType = WGPUTextureSampleType_Float;
      out[n].texture.viewDimension = WGPUTextureViewDimension_2D;
      (*texCount)++;
    } else {
      out[n].visibility = WGPUShaderStage_Fragment;
      out[n].sampler.type = WGPUSamplerBindingType_Filtering;
    }
    if (*s == ';') s++;
    n++;
  }
  return n;
}

static int parse_attrs(const char *s, WGPUVertexAttribute *out) {
  int n = 0;
  while (*s && n < MAX_ATTRS) {
    int loc, off, comp;
    s = spec_int(s, &loc);
    s = spec_int(s, &off);
    s = spec_int(s, &comp);
    out[n].shaderLocation = (uint32_t)loc;
    out[n].offset = (uint64_t)off;
    out[n].format = f32_format(comp);
    if (*s == ';') s++;
    n++;
  }
  return n;
}

static const int b64tab[256] = {
  ['A']=0,['B']=1,['C']=2,['D']=3,['E']=4,['F']=5,['G']=6,['H']=7,['I']=8,
  ['J']=9,['K']=10,['L']=11,['M']=12,['N']=13,['O']=14,['P']=15,['Q']=16,
  ['R']=17,['S']=18,['T']=19,['U']=20,['V']=21,['W']=22,['X']=23,['Y']=24,
  ['Z']=25,['a']=26,['b']=27,['c']=28,['d']=29,['e']=30,['f']=31,['g']=32,
  ['h']=33,['i']=34,['j']=35,['k']=36,['l']=37,['m']=38,['n']=39,['o']=40,
  ['p']=41,['q']=42,['r']=43,['s']=44,['t']=45,['u']=46,['v']=47,['w']=48,
  ['x']=49,['y']=50,['z']=51,['0']=52,['1']=53,['2']=54,['3']=55,['4']=56,
  ['5']=57,['6']=58,['7']=59,['8']=60,['9']=61,['+']=62,['/']=63,
};

static unsigned char *b64_decode(const char *s, size_t *outLen) {
  size_t len = strlen(s);
  unsigned char *out = (unsigned char *)malloc(len / 4 * 3 + 4);
  size_t o = 0;
  unsigned acc = 0;
  int bits = 0;
  for (size_t i = 0; i < len; i++) {
    unsigned char c = (unsigned char)s[i];
    if (c == '=' || c == '\n' || c == '\r') continue;
    acc = (acc << 6) | (unsigned)b64tab[c];
    bits += 6;
    if (bits >= 8) {
      bits -= 8;
      out[o++] = (unsigned char)(acc >> bits);
    }
  }
  *outLen = o;
  return out;
}

/* ----------------------------------------------------- adapter and surface */

static void on_adapter(WGPURequestAdapterStatus st, WGPUAdapter a,
                       WGPUStringView msg, void *u1, void *u2) {
  (void)u2;
  if (st == WGPURequestAdapterStatus_Success) *(WGPUAdapter *)u1 = a;
  else fprintf(stderr, "offler: no adapter: %.*s\n", (int)msg.length,
               msg.data ? msg.data : "");
}

static void on_device(WGPURequestDeviceStatus st, WGPUDevice d,
                      WGPUStringView msg, void *u1, void *u2) {
  (void)u2;
  if (st == WGPURequestDeviceStatus_Success) *(WGPUDevice *)u1 = d;
  else fprintf(stderr, "offler: no device: %.*s\n", (int)msg.length,
               msg.data ? msg.data : "");
}

/* The one genuinely per-platform function. SDL3 hands back native handles
 * through its property store; on macOS it also builds the CAMetalLayer that
 * wgpu wants, which is what keeps Objective-C out of this project. */
static WGPUSurface make_surface(WGPUInstance inst, SDL_Window *w) {
  SDL_PropertiesID props = SDL_GetWindowProperties(w);
  const char *driver = SDL_GetCurrentVideoDriver();

#ifdef __APPLE__
  {
    SDL_MetalView view = SDL_Metal_CreateView(w);
    WGPUSurfaceSourceMetalLayer src = {
      .chain = { .sType = WGPUSType_SurfaceSourceMetalLayer },
      .layer = SDL_Metal_GetLayer(view),
    };
    WGPUSurfaceDescriptor d = { .nextInChain = &src.chain };
    return wgpuInstanceCreateSurface(inst, &d);
  }
#else
  if (driver && strcmp(driver, "wayland") == 0) {
    void *display = SDL_GetPointerProperty(
        props, SDL_PROP_WINDOW_WAYLAND_DISPLAY_POINTER, NULL);
    void *surface = SDL_GetPointerProperty(
        props, SDL_PROP_WINDOW_WAYLAND_SURFACE_POINTER, NULL);
    WGPUSurfaceSourceWaylandSurface src = {
      .chain = { .sType = WGPUSType_SurfaceSourceWaylandSurface },
      .display = display, .surface = surface,
    };
    WGPUSurfaceDescriptor d = { .nextInChain = &src.chain };
    return wgpuInstanceCreateSurface(inst, &d);
  } else {
    void *display = SDL_GetPointerProperty(
        props, SDL_PROP_WINDOW_X11_DISPLAY_POINTER, NULL);
    Sint64 window = SDL_GetNumberProperty(
        props, SDL_PROP_WINDOW_X11_WINDOW_NUMBER, 0);
    WGPUSurfaceSourceXlibWindow src = {
      .chain = { .sType = WGPUSType_SurfaceSourceXlibWindow },
      .display = display, .window = (uint64_t)window,
    };
    WGPUSurfaceDescriptor d = { .nextInChain = &src.chain };
    return wgpuInstanceCreateSurface(inst, &d);
  }
#endif
}

static void configure(Ctx *c) {
  SDL_GetWindowSizeInPixels(c->window, &c->width, &c->height);
  if (c->width < 1) c->width = 1;
  if (c->height < 1) c->height = 1;

  WGPUSurfaceConfiguration cfg = {
    .device = c->device,
    .format = c->format,
    .usage = WGPUTextureUsage_RenderAttachment,
    .width = (uint32_t)c->width,
    .height = (uint32_t)c->height,
    .alphaMode = WGPUCompositeAlphaMode_Auto,
    .presentMode = WGPUPresentMode_Fifo,
  };
  wgpuSurfaceConfigure(c->surface, &cfg);

  /* The depth attachment does not follow the surface, so it is rebuilt with
   * it -- the same trap the browser build hits on canvas resize. */
  if (c->depthView) wgpuTextureViewRelease(c->depthView);
  if (c->depth) wgpuTextureRelease(c->depth);
  WGPUTextureDescriptor td = {
    .usage = WGPUTextureUsage_RenderAttachment,
    .dimension = WGPUTextureDimension_2D,
    .size = { (uint32_t)c->width, (uint32_t)c->height, 1 },
    .format = WGPUTextureFormat_Depth24Plus,
    .mipLevelCount = 1,
    .sampleCount = 1,
  };
  c->depth = wgpuDeviceCreateTexture(c->device, &td);
  c->depthView = wgpuTextureCreateView(c->depth, NULL);
}

/* --------------------------------------------------------------- pipelines */

static WGPURenderPipeline make_pipeline(Ctx *c, WGPUShaderModule mod,
                                        WGPUPipelineLayout layout,
                                        WGPUVertexAttribute *attrs, int nattrs,
                                        int stride, int lineTopology,
                                        int blend) {
  WGPUVertexBufferLayout vbl = { .stepMode = WGPUVertexStepMode_Vertex,
                                 .arrayStride = (uint64_t)stride,
                                 .attributeCount = (size_t)nattrs,
                                 .attributes = attrs };
  WGPUBlendState blendState = {
    .color = { .operation = WGPUBlendOperation_Add,
               .srcFactor = WGPUBlendFactor_SrcAlpha,
               .dstFactor = WGPUBlendFactor_OneMinusSrcAlpha },
    .alpha = { .operation = WGPUBlendOperation_Add,
               .srcFactor = WGPUBlendFactor_One,
               .dstFactor = WGPUBlendFactor_OneMinusSrcAlpha },
  };
  WGPUColorTargetState target = { .format = c->format,
                                  .blend = blend ? &blendState : NULL,
                                  .writeMask = WGPUColorWriteMask_All };
  WGPUFragmentState frag = { .module = mod, .entryPoint = SV("fs"),
                             .targetCount = 1, .targets = &target };
  WGPUDepthStencilState depth = {
    .format = WGPUTextureFormat_Depth24Plus,
    .depthWriteEnabled = blend ? WGPUOptionalBool_False : WGPUOptionalBool_True,
    .depthCompare = WGPUCompareFunction_Less,
    .stencilFront = { .compare = WGPUCompareFunction_Always },
    .stencilBack  = { .compare = WGPUCompareFunction_Always },
  };
  WGPURenderPipelineDescriptor rpd = {
    .layout = layout,
    .vertex = { .module = mod, .entryPoint = SV("vs"),
                .bufferCount = 1, .buffers = &vbl },
    .primitive = { .topology = lineTopology ? WGPUPrimitiveTopology_LineList
                                            : WGPUPrimitiveTopology_TriangleList,
                   .frontFace = WGPUFrontFace_CCW,
                   .cullMode = lineTopology ? WGPUCullMode_None
                                            : WGPUCullMode_Back },
    .depthStencil = &depth,
    .multisample = { .count = 1, .mask = 0xFFFFFFFF },
    .fragment = &frag,
  };
  return wgpuDeviceCreateRenderPipeline(c->device, &rpd);
}

static WGPURenderPipeline make_line_pipeline_impl(Ctx *c, WGPUShaderModule mod,
                                                  WGPUPipelineLayout layout,
                                                  int blend) {
  WGPUVertexBufferLayout vbl = { .stepMode = WGPUVertexStepMode_Vertex,
                                 .arrayStride = (uint64_t)c->lineStride,
                                 .attributeCount = (size_t)c->lineAttrCount,
                                 .attributes = c->lineAttrs };
  WGPUBlendState blendState = {
    .color = { .operation = WGPUBlendOperation_Add,
               .srcFactor = WGPUBlendFactor_SrcAlpha,
               .dstFactor = WGPUBlendFactor_OneMinusSrcAlpha },
    .alpha = { .operation = WGPUBlendOperation_Add,
               .srcFactor = WGPUBlendFactor_One,
               .dstFactor = WGPUBlendFactor_OneMinusSrcAlpha },
  };
  WGPUColorTargetState target = { .format = c->format,
                                  .blend = blend ? &blendState : NULL,
                                  .writeMask = WGPUColorWriteMask_All };
  WGPUFragmentState frag = { .module = mod, .entryPoint = SV("fs"),
                             .targetCount = 1, .targets = &target };
  WGPUDepthStencilState depth = {
    .format = WGPUTextureFormat_Depth24Plus,
    .depthWriteEnabled = blend ? WGPUOptionalBool_False : WGPUOptionalBool_True,
    .depthCompare = WGPUCompareFunction_Less,
    .stencilFront = { .compare = WGPUCompareFunction_Always },
    .stencilBack  = { .compare = WGPUCompareFunction_Always },
  };
  WGPURenderPipelineDescriptor rpd = {
    .layout = layout,
    .vertex = { .module = mod, .entryPoint = SV("vs_line"),
                .bufferCount = 1, .buffers = &vbl },
    .primitive = { .topology = WGPUPrimitiveTopology_LineList,
                   .frontFace = WGPUFrontFace_CCW,
                   .cullMode = WGPUCullMode_None },
    .depthStencil = &depth,
    .multisample = { .count = 1, .mask = 0xFFFFFFFF },
    .fragment = &frag,
  };
  return wgpuDeviceCreateRenderPipeline(c->device, &rpd);
}

static WGPURenderPipeline make_line_pipeline(Ctx *c, WGPUShaderModule mod,
                                             WGPUPipelineLayout layout,
                                             int blend) {
  return make_line_pipeline_impl(c, mod, layout, blend);
}

/* A line-topology variant from a material module's vs_line entry point. */
static WGPURenderPipeline make_line_pipeline(Ctx *c, WGPUShaderModule mod,
                                             WGPUPipelineLayout layout,
                                             int blend);

/* --------------------------------------------------------------------- init */

void *offler_init(const char *title,
                  const char *lineWgsl, const char *lineBindSpec,
                  const char *lineVertSpec, int lineStride,
                  const char *meshVertSpec, int meshStride,
                  int globalSize, int objSize, int objStride, int maxObjects) {
  Ctx *c = (Ctx *)calloc(1, sizeof(Ctx));
  c->globalSize = globalSize;
  c->objSize = objSize;
  c->objStride = objStride;
  c->maxObjects = maxObjects;
  c->meshStride = meshStride;
  c->lineStride = lineStride;
  c->meshAttrCount = parse_attrs(meshVertSpec, c->meshAttrs);
  c->lineAttrCount = parse_attrs(lineVertSpec, c->lineAttrs);

  if (!SDL_Init(SDL_INIT_VIDEO)) {
    fprintf(stderr, "offler: SDL_Init: %s\n", SDL_GetError());
    free(c); return NULL;
  }
  c->window = SDL_CreateWindow(title, 1280, 800,
                               SDL_WINDOW_RESIZABLE | SDL_WINDOW_HIGH_PIXEL_DENSITY);
  if (!c->window) {
    fprintf(stderr, "offler: SDL_CreateWindow: %s\n", SDL_GetError());
    free(c); return NULL;
  }

  c->instance = wgpuCreateInstance(NULL);
  c->surface = make_surface(c->instance, c->window);
  if (!c->surface) { fprintf(stderr, "offler: no surface\n"); free(c); return NULL; }

  WGPURequestAdapterOptions opts = { .compatibleSurface = c->surface };
  WGPURequestAdapterCallbackInfo aci = {
    .mode = WGPUCallbackMode_AllowProcessEvents,
    .callback = on_adapter, .userdata1 = &c->adapter,
  };
  wgpuInstanceRequestAdapter(c->instance, &opts, aci);
  for (int i = 0; i < 2000 && !c->adapter; i++) wgpuInstanceProcessEvents(c->instance);
  if (!c->adapter) { free(c); return NULL; }

  WGPURequestDeviceCallbackInfo dci = {
    .mode = WGPUCallbackMode_AllowProcessEvents,
    .callback = on_device, .userdata1 = &c->device,
  };
  wgpuAdapterRequestDevice(c->adapter, NULL, dci);
  for (int i = 0; i < 2000 && !c->device; i++) wgpuInstanceProcessEvents(c->instance);
  if (!c->device) { free(c); return NULL; }

  c->queue = wgpuDeviceGetQueue(c->device);

  WGPUSurfaceCapabilities caps = {0};
  wgpuSurfaceGetCapabilities(c->surface, c->adapter, &caps);
  c->format = caps.formatCount ? caps.formats[0] : WGPUTextureFormat_BGRA8Unorm;

  configure(c);

  WGPUBufferDescriptor gbd = { .usage = WGPUBufferUsage_Uniform | WGPUBufferUsage_CopyDst,
                               .size = (uint64_t)globalSize };
  c->globalBuf = wgpuDeviceCreateBuffer(c->device, &gbd);
  WGPUBufferDescriptor obd = { .usage = WGPUBufferUsage_Uniform | WGPUBufferUsage_CopyDst,
                               .size = (uint64_t)objStride * maxObjects };
  c->objBuf = wgpuDeviceCreateBuffer(c->device, &obd);
  c->matBuf = wgpuDeviceCreateBuffer(c->device, &obd);

  WGPUSamplerDescriptor sd = {
    .addressModeU = WGPUAddressMode_Repeat,
    .addressModeV = WGPUAddressMode_Repeat,
    .addressModeW = WGPUAddressMode_Repeat,
    .magFilter = WGPUFilterMode_Linear,
    .minFilter = WGPUFilterMode_Linear,
    .mipmapFilter = WGPUMipmapFilterMode_Nearest,
    .maxAnisotropy = 1,
  };
  c->sampler = wgpuDeviceCreateSampler(c->device, &sd);

  /* The 1x1 white every unmapped texture slot binds, so an absent map
   * multiplies by one. */
  {
    WGPUTextureDescriptor td = {
      .usage = WGPUTextureUsage_TextureBinding | WGPUTextureUsage_CopyDst,
      .dimension = WGPUTextureDimension_2D,
      .size = { 1, 1, 1 },
      .format = WGPUTextureFormat_RGBA8Unorm,
      .mipLevelCount = 1,
      .sampleCount = 1,
    };
    WGPUTexture t = wgpuDeviceCreateTexture(c->device, &td);
    unsigned char px[4] = { 255, 255, 255, 255 };
    WGPUTexelCopyTextureInfo dst = { .texture = t };
    WGPUTexelCopyBufferLayout lay = { .bytesPerRow = 4, .rowsPerImage = 1 };
    WGPUExtent3D ext = { 1, 1, 1 };
    wgpuQueueWriteTexture(c->queue, &dst, px, 4, &lay, &ext);
    c->white = wgpuTextureCreateView(t, NULL);
  }

  /* The engine's line pipeline: generated WGSL against the engine blocks,
   * whose lane carries the colour. */
  {
    WGPUShaderSourceWGSL src = {
      .chain = { .sType = WGPUSType_ShaderSourceWGSL },
      .code = SV(lineWgsl),
    };
    WGPUShaderModuleDescriptor smd = { .nextInChain = &src.chain };
    WGPUShaderModule mod = wgpuDeviceCreateShaderModule(c->device, &smd);
    WGPUBindGroupLayoutEntry entries[MAX_BINDINGS];
    int msz, tc;
    int nb = parse_bind_spec(lineBindSpec, entries, &msz, &tc);
    WGPUBindGroupLayoutDescriptor bgld = { .entryCount = (size_t)nb,
                                           .entries = entries };
    WGPUBindGroupLayout bgl = wgpuDeviceCreateBindGroupLayout(c->device, &bgld);
    WGPUPipelineLayoutDescriptor pld = { .bindGroupLayoutCount = 1,
                                         .bindGroupLayouts = &bgl };
    WGPUPipelineLayout layout = wgpuDeviceCreatePipelineLayout(c->device, &pld);
    c->linePipeline = make_pipeline(c, mod, layout, c->lineAttrs, c->lineAttrCount,
                                    lineStride, 1, 1);
    WGPUBindGroupEntry bge[2] = {
      { .binding = 0, .buffer = c->globalBuf, .size = (uint64_t)globalSize },
      { .binding = 1, .buffer = c->objBuf, .size = (uint64_t)objSize },
    };
    WGPUBindGroupDescriptor bgd = { .layout = bgl, .entryCount = 2, .entries = bge };
    c->lineBindGroup = wgpuDeviceCreateBindGroup(c->device, &bgd);
  }

  c->meshCap = 16;
  c->meshes = (Mesh *)calloc((size_t)c->meshCap, sizeof(Mesh));
  c->texCap = 16;
  c->texs = (WGPUTextureView *)calloc((size_t)c->texCap, sizeof(WGPUTextureView));
  c->matCap = 8;
  c->mats = (Mat *)calloc((size_t)c->matCap, sizeof(Mat));
  c->pendCap = 64;
  c->pend = (Pending *)calloc((size_t)c->pendCap, sizeof(Pending));
  c->assetCap = 16;
  c->assets = (struct Asset *)calloc((size_t)c->assetCap, sizeof(struct Asset));

  return c;
}

/* ------------------------------------------------------------- registration */

int offler_register_material(void *p, const char *wgsl, const char *bindSpec,
                             int hasLine) {
  Ctx *c = (Ctx *)p;
  if (c->matCount == c->matCap) {
    c->matCap *= 2;
    c->mats = (Mat *)realloc(c->mats, (size_t)c->matCap * sizeof(Mat));
  }
  Mat *m = &c->mats[c->matCount];
  memset(m, 0, sizeof(*m));

  WGPUShaderSourceWGSL src = {
    .chain = { .sType = WGPUSType_ShaderSourceWGSL },
    .code = SV(wgsl),
  };
  WGPUShaderModuleDescriptor smd = { .nextInChain = &src.chain };
  WGPUShaderModule mod = wgpuDeviceCreateShaderModule(c->device, &smd);

  WGPUBindGroupLayoutEntry entries[MAX_BINDINGS];
  int nb = parse_bind_spec(bindSpec, entries, &m->matSize, &m->texCount);
  WGPUBindGroupLayoutDescriptor bgld = { .entryCount = (size_t)nb,
                                         .entries = entries };
  m->bgl = wgpuDeviceCreateBindGroupLayout(c->device, &bgld);
  WGPUPipelineLayoutDescriptor pld = { .bindGroupLayoutCount = 1,
                                       .bindGroupLayouts = &m->bgl };
  WGPUPipelineLayout layout = wgpuDeviceCreatePipelineLayout(c->device, &pld);

  m->triO = make_pipeline(c, mod, layout, c->meshAttrs, c->meshAttrCount,
                          c->meshStride, 0, 0);
  m->triB = make_pipeline(c, mod, layout, c->meshAttrs, c->meshAttrCount,
                          c->meshStride, 0, 1);
  if (hasLine) {
    m->lineO = make_line_pipeline(c, mod, layout, 0);
    m->lineB = make_line_pipeline(c, mod, layout, 1);
  }
  m->bgCap = 4;
  m->bgs = (BGEntry *)calloc((size_t)m->bgCap, sizeof(BGEntry));
  return c->matCount++;
}

/* ---------------------------------------------------------------- textures */

static int add_texture(Ctx *c, unsigned char *rgba, int w, int h) {
  WGPUTextureDescriptor td = {
    .usage = WGPUTextureUsage_TextureBinding | WGPUTextureUsage_CopyDst,
    .dimension = WGPUTextureDimension_2D,
    .size = { (uint32_t)w, (uint32_t)h, 1 },
    .format = WGPUTextureFormat_RGBA8UnormSrgb,
    .mipLevelCount = 1,
    .sampleCount = 1,
  };
  WGPUTexture t = wgpuDeviceCreateTexture(c->device, &td);
  WGPUTexelCopyTextureInfo dst = { .texture = t };
  WGPUTexelCopyBufferLayout lay = { .bytesPerRow = (uint32_t)(4 * w),
                                    .rowsPerImage = (uint32_t)h };
  WGPUExtent3D ext = { (uint32_t)w, (uint32_t)h, 1 };
  wgpuQueueWriteTexture(c->queue, &dst, rgba, (size_t)(4 * w * h), &lay, &ext);
  if (c->texCount == c->texCap) {
    c->texCap *= 2;
    c->texs = (WGPUTextureView *)realloc(c->texs,
                (size_t)c->texCap * sizeof(WGPUTextureView));
  }
  c->texs[c->texCount] = wgpuTextureCreateView(t, NULL);
  return c->texCount++;
}

/* A decode failure still yields a valid handle -- bound to the shared 1x1
 * white, matching the browser backends' texture-not-yet-loaded behaviour. */
static int white_entry(Ctx *c) {
  if (c->texCount == c->texCap) {
    c->texCap *= 2;
    c->texs = (WGPUTextureView *)realloc(c->texs,
                (size_t)c->texCap * sizeof(WGPUTextureView));
  }
  c->texs[c->texCount] = c->white;
  return c->texCount++;
}

int offler_texture_file(void *p, const char *path) {
  Ctx *c = (Ctx *)p;
  int w, h, n;
  unsigned char *rgba = stbi_load(path, &w, &h, &n, 4);
  if (!rgba) {
    fprintf(stderr, "offler: could not decode %s: %s\n", path, stbi_failure_reason());
    return white_entry(c);
  }
  int id = add_texture(c, rgba, w, h);
  stbi_image_free(rgba);
  return id;
}

int offler_texture_b64(void *p, const char *b64) {
  Ctx *c = (Ctx *)p;
  size_t len;
  unsigned char *bytes = b64_decode(b64, &len);
  int w, h, n;
  unsigned char *rgba = stbi_load_from_memory(bytes, (int)len, &w, &h, &n, 4);
  free(bytes);
  if (!rgba) {
    fprintf(stderr, "offler: could not decode embedded image: %s\n",
            stbi_failure_reason());
    return white_entry(c);
  }
  int id = add_texture(c, rgba, w, h);
  stbi_image_free(rgba);
  return id;
}

/* --------------------------------------------------------- window & input */

void offler_set_title(void *p, const char *s) {
  SDL_SetWindowTitle(((Ctx *)p)->window, s);
}

double offler_time(void *p) { (void)p; return (double)SDL_GetTicks() * 0.001; }

double offler_aspect(void *p) {
  Ctx *c = (Ctx *)p;
  return (double)c->width / (double)c->height;
}

/* SDL key names, translated to the browser's `KeyboardEvent.key` vocabulary
 * so a scene matches on one spelling. */
static void set_key(Ctx *c, SDL_Keycode key) {
  const char *n = SDL_GetKeyName(key);
  if (!n) n = "";
  if (strcmp(n, "Left") == 0) n = "ArrowLeft";
  else if (strcmp(n, "Right") == 0) n = "ArrowRight";
  else if (strcmp(n, "Up") == 0) n = "ArrowUp";
  else if (strcmp(n, "Down") == 0) n = "ArrowDown";
  else if (strcmp(n, "Space") == 0) n = " ";
  else if (strcmp(n, "Return") == 0) n = "Enter";
  snprintf(c->eventKey, sizeof(c->eventKey), "%s", n);
  if (c->eventKey[0] && !c->eventKey[1]
      && c->eventKey[0] >= 'A' && c->eventKey[0] <= 'Z')
    c->eventKey[0] += 'a' - 'A';
}

static void set_pointer(Ctx *c, float x, float y) {
  int ww = 1, wh = 1;
  SDL_GetWindowSize(c->window, &ww, &wh);
  if (ww < 1) ww = 1;
  if (wh < 1) wh = 1;
  c->eventX = (double)x * c->width / ww;
  c->eventY = (double)y * c->height / wh;
}

/* One event per call, 0 when the queue is empty.
 * 1 close, 2 resize, 3 keydown, 4 keyup, 5 move, 6 down, 7 up, 8 wheel. */
int offler_poll(void *p) {
  Ctx *c = (Ctx *)p;
  SDL_Event e;
  while (SDL_PollEvent(&e)) {
    switch (e.type) {
      case SDL_EVENT_QUIT: return 1;
      case SDL_EVENT_WINDOW_CLOSE_REQUESTED: return 1;
      case SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED: return 2;
      case SDL_EVENT_KEY_DOWN: set_key(c, e.key.key); return 3;
      case SDL_EVENT_KEY_UP: set_key(c, e.key.key); return 4;
      case SDL_EVENT_MOUSE_MOTION:
        set_pointer(c, e.motion.x, e.motion.y); return 5;
      case SDL_EVENT_MOUSE_BUTTON_DOWN:
      case SDL_EVENT_MOUSE_BUTTON_UP:
        set_pointer(c, e.button.x, e.button.y);
        c->eventButton = e.button.button == SDL_BUTTON_LEFT ? 0
                       : e.button.button == SDL_BUTTON_MIDDLE ? 1
                       : e.button.button == SDL_BUTTON_RIGHT ? 2
                       : (int)e.button.button;
        return e.type == SDL_EVENT_MOUSE_BUTTON_DOWN ? 6 : 7;
      case SDL_EVENT_MOUSE_WHEEL:
        c->eventWheel = (double)e.wheel.y;
        return 8;
      default: break;
    }
  }
  return 0;
}

const char *offler_event_key(void *p) { return ((Ctx *)p)->eventKey; }
double offler_event_x(void *p) { return ((Ctx *)p)->eventX; }
double offler_event_y(void *p) { return ((Ctx *)p)->eventY; }
double offler_event_wheel(void *p) { return ((Ctx *)p)->eventWheel; }
int offler_event_button(void *p) { return ((Ctx *)p)->eventButton; }

void offler_resize(void *p) { configure((Ctx *)p); }

/* ----------------------------------------------------------------- meshes */

static int push_mesh(Ctx *c, Mesh m) {
  if (c->meshCount == c->meshCap) {
    c->meshCap *= 2;
    c->meshes = (Mesh *)realloc(c->meshes, (size_t)c->meshCap * sizeof(Mesh));
  }
  c->meshes[c->meshCount] = m;
  return c->meshCount++;
}

static WGPUBuffer upload_static(Ctx *c, const void *data, size_t bytes,
                                WGPUBufferUsage usage) {
  WGPUBufferDescriptor bd = { .usage = usage | WGPUBufferUsage_CopyDst,
                              .size = bytes };
  WGPUBuffer buf = wgpuDeviceCreateBuffer(c->device, &bd);
  wgpuQueueWriteBuffer(c->queue, buf, 0, data, bytes);
  return buf;
}

int offler_create_mesh(void *p, float *verts, int vertexCount, int topo) {
  Ctx *c = (Ctx *)p;
  int floats = vertexCount * (topo == 1 ? c->lineStride : c->meshStride) / 4;
  WGPUBuffer buf = upload_static(c, verts, (size_t)floats * sizeof(float),
                                 WGPUBufferUsage_Vertex);
  return push_mesh(c, (Mesh){ .buf = buf, .count = vertexCount, .topo = topo,
                              .ibuf = NULL, .icount = 0 });
}

int offler_create_mesh_indexed(void *p, float *verts, int vertexCount,
                               unsigned *idx, int idxCount) {
  Ctx *c = (Ctx *)p;
  int floats = vertexCount * c->meshStride / 4;
  WGPUBuffer buf = upload_static(c, verts, (size_t)floats * sizeof(float),
                                 WGPUBufferUsage_Vertex);
  WGPUBuffer ib = upload_static(c, idx, (size_t)idxCount * sizeof(unsigned),
                                WGPUBufferUsage_Index);
  return push_mesh(c, (Mesh){ .buf = buf, .count = vertexCount, .topo = 0,
                              .ibuf = ib, .icount = idxCount });
}

/* Destroy the buffers and tombstone the entry; exec_draw skips draws whose
 * entry is gone, so stale handles are silent, not fatal. Indices are never
 * reused. */
void offler_free_mesh(void *p, int mi) {
  Ctx *c = (Ctx *)p;
  if (mi < 0 || mi >= c->meshCount) return;
  Mesh *m = &c->meshes[mi];
  if (m->buf) {
    wgpuBufferDestroy(m->buf);
    wgpuBufferRelease(m->buf);
    m->buf = NULL;
  }
  if (m->ibuf) {
    wgpuBufferDestroy(m->ibuf);
    wgpuBufferRelease(m->ibuf);
    m->ibuf = NULL;
  }
  m->count = 0;
  m->icount = 0;
}

void offler_set_lines(void *p, float *verts, int floatCount, int vertexCount) {
  Ctx *c = (Ctx *)p;
  if (c->lineBuf) { wgpuBufferDestroy(c->lineBuf); wgpuBufferRelease(c->lineBuf); }
  c->lineBuf = NULL;
  c->lineCount = vertexCount;
  if (vertexCount <= 0) return;
  size_t bytes = (size_t)floatCount * sizeof(float);
  WGPUBufferDescriptor bd = { .usage = WGPUBufferUsage_Vertex | WGPUBufferUsage_CopyDst,
                              .size = bytes };
  c->lineBuf = wgpuDeviceCreateBuffer(c->device, &bd);
  wgpuQueueWriteBuffer(c->queue, c->lineBuf, 0, verts, bytes);
}

/* ---------------------------------------------------------------- drawing */

/* The bind group for a material and a texture set, cached: textures are
 * never freed, so entries live for the process. A linear scan, but the
 * cache holds one entry per distinct texture set per material. */
static WGPUBindGroup bg_for(Ctx *c, int matIdx, const int *tex) {
  Mat *m = &c->mats[matIdx];
  for (int i = 0; i < m->bgCount; i++)
    if (memcmp(m->bgs[i].tex, tex, sizeof(int) * MAX_TEX_SLOTS) == 0)
      return m->bgs[i].bg;

  WGPUBindGroupEntry es[MAX_BINDINGS];
  memset(es, 0, sizeof(es));
  es[0] = (WGPUBindGroupEntry){ .binding = 0, .buffer = c->globalBuf,
                                .size = (uint64_t)c->globalSize };
  es[1] = (WGPUBindGroupEntry){ .binding = 1, .buffer = c->objBuf,
                                .size = (uint64_t)c->objSize };
  es[2] = (WGPUBindGroupEntry){ .binding = 2, .buffer = c->matBuf,
                                .size = (uint64_t)m->matSize };
  int n = 3;
  for (int i = 0; i < m->texCount; i++) {
    WGPUTextureView v = (tex[i] >= 0 && tex[i] < c->texCount)
                          ? c->texs[tex[i]] : c->white;
    es[n++] = (WGPUBindGroupEntry){ .binding = (uint32_t)(3 + 2 * i),
                                    .textureView = v };
    es[n++] = (WGPUBindGroupEntry){ .binding = (uint32_t)(4 + 2 * i),
                                    .sampler = c->sampler };
  }
  WGPUBindGroupDescriptor bgd = { .layout = m->bgl, .entryCount = (size_t)n,
                                  .entries = es };
  WGPUBindGroup bg = wgpuDeviceCreateBindGroup(c->device, &bgd);

  if (m->bgCount == m->bgCap) {
    m->bgCap *= 2;
    m->bgs = (BGEntry *)realloc(m->bgs, (size_t)m->bgCap * sizeof(BGEntry));
  }
  memcpy(m->bgs[m->bgCount].tex, tex, sizeof(int) * MAX_TEX_SLOTS);
  m->bgs[m->bgCount].bg = bg;
  return m->bgs[m->bgCount++].bg;
}

int offler_add_asset(void *p, int mat, int slot,
                     int t0, int t1, int t2, int t3) {
  Ctx *c = (Ctx *)p;
  if (c->assetCount == c->assetCap) {
    c->assetCap *= 2;
    c->assets = (struct Asset *)realloc(c->assets,
                  (size_t)c->assetCap * sizeof(struct Asset));
  }
  struct Asset *a = &c->assets[c->assetCount];
  a->mat = mat;
  a->slot = slot;
  a->tex[0] = t0; a->tex[1] = t1; a->tex[2] = t2; a->tex[3] = t3;
  a->bg = bg_for(c, mat, a->tex);
  return c->assetCount++;
}

void offler_update_asset(void *p, int ai, int t0, int t1, int t2, int t3) {
  Ctx *c = (Ctx *)p;
  if (ai < 0 || ai >= c->assetCount) return;
  struct Asset *a = &c->assets[ai];
  a->tex[0] = t0; a->tex[1] = t1; a->tex[2] = t2; a->tex[3] = t3;
  a->bg = bg_for(c, a->mat, a->tex);
}

/* One asset's 256-byte slot, uploaded when the asset is made or updated --
 * never per draw. */
void offler_upload_mat_slot(void *p, float *matData, int slot) {
  Ctx *c = (Ctx *)p;
  wgpuQueueWriteBuffer(c->queue, c->matBuf,
                       (uint64_t)slot * c->objStride,
                       matData + (size_t)slot * (c->objStride / 4),
                       (size_t)c->objStride);
}

static void exec_draw(Ctx *c, const Pending *d, int blend) {
  struct Asset *a = &c->assets[d->asset];
  Mat *m = &c->mats[a->mat];
  Mesh *mm = &c->meshes[d->mesh];
  if (!mm->buf) return;
  WGPURenderPipeline pipe = mm->topo == 1 ? (blend ? m->lineB : m->lineO)
                                          : (blend ? m->triB : m->triO);
  if (!pipe) return;
  if (pipe != c->boundPipe) {
    wgpuRenderPassEncoderSetPipeline(c->pass, pipe);
    c->boundPipe = pipe;
  }
  if (mm->buf != c->boundVerts) {
    wgpuRenderPassEncoderSetVertexBuffer(c->pass, 0, mm->buf, 0, WGPU_WHOLE_SIZE);
    if (mm->ibuf)
      wgpuRenderPassEncoderSetIndexBuffer(c->pass, mm->ibuf,
                                          WGPUIndexFormat_Uint32, 0,
                                          WGPU_WHOLE_SIZE);
    c->boundVerts = mm->buf;
  }
  uint32_t offs[2] = { (uint32_t)(d->slot * c->objStride),
                       (uint32_t)(a->slot * c->objStride) };
  wgpuRenderPassEncoderSetBindGroup(c->pass, 0, a->bg, 2, offs);
  if (mm->ibuf)
    wgpuRenderPassEncoderDrawIndexed(c->pass, (uint32_t)mm->icount, 1, 0, 0, 0);
  else
    wgpuRenderPassEncoderDraw(c->pass, (uint32_t)mm->count, 1, 0, 0);
}

static void push_pending(Ctx *c, const Pending *d) {
  if (c->pendCount == c->pendCap) {
    c->pendCap *= 2;
    c->pend = (Pending *)realloc(c->pend, (size_t)c->pendCap * sizeof(Pending));
  }
  c->pend[c->pendCount++] = *d;
}

void offler_draw(void *p, int asset, int mesh, int slot, int blend,
                 double depth) {
  Ctx *c = (Ctx *)p;
  if (!c->pass || asset < 0 || asset >= c->assetCount
      || mesh < 0 || mesh >= c->meshCount || slot >= c->maxObjects) return;
  Pending d = { .asset = asset, .mesh = mesh, .slot = slot, .depth = depth };
  if (blend) push_pending(c, &d);
  else exec_draw(c, &d, 0);
}

void offler_draw_slices(void *p, int asset, int mesh, int first, int count,
                        int blend, double depth) {
  Ctx *c = (Ctx *)p;
  if (!c->pass || asset < 0 || asset >= c->assetCount
      || mesh < 0 || mesh >= c->meshCount) return;
  for (int i = 0; i < count; i++) {
    if (first + i >= c->maxObjects) return;
    Pending d = { .asset = asset, .mesh = mesh, .slot = first + i,
                  .depth = depth };
    if (blend) push_pending(c, &d);
    else exec_draw(c, &d, 0);
  }
}

void offler_draw_lines(void *p, int slot) {
  Ctx *c = (Ctx *)p;
  if (!c->pass || !c->lineBuf || c->lineCount <= 0 || slot >= c->maxObjects) return;
  uint32_t off = (uint32_t)(slot * c->objStride);
  wgpuRenderPassEncoderSetPipeline(c->pass, c->linePipeline);
  wgpuRenderPassEncoderSetVertexBuffer(c->pass, 0, c->lineBuf, 0, WGPU_WHOLE_SIZE);
  wgpuRenderPassEncoderSetBindGroup(c->pass, 0, c->lineBindGroup, 1, &off);
  wgpuRenderPassEncoderDraw(c->pass, (uint32_t)c->lineCount, 1, 0, 0);
  c->boundPipe = NULL;
  c->boundVerts = NULL;
}

/* Back to front, so blends composite correctly. */
static int pend_cmp(const void *a, const void *b) {
  double da = ((const Pending *)a)->depth, db = ((const Pending *)b)->depth;
  return da < db ? 1 : da > db ? -1 : 0;
}

/* Returns 0 if the frame cannot be started, so Idris can skip it. */
int offler_begin(void *p, float *globals, double r, double g, double b) {
  Ctx *c = (Ctx *)p;

  /* Absorb resizes here, so applications never handle them: the surface
   * and the depth attachment follow the window together. */
  int w = 0, h = 0;
  SDL_GetWindowSizeInPixels(c->window, &w, &h);
  if (w != c->width || h != c->height) configure(c);

  wgpuQueueWriteBuffer(c->queue, c->globalBuf, 0, globals, (size_t)c->globalSize);

  WGPUSurfaceTexture st = {0};
  wgpuSurfaceGetCurrentTexture(c->surface, &st);
  if (st.status != WGPUSurfaceGetCurrentTextureStatus_SuccessOptimal &&
      st.status != WGPUSurfaceGetCurrentTextureStatus_SuccessSuboptimal) {
    if (st.texture) wgpuTextureRelease(st.texture);
    configure(c);
    return 0;
  }
  c->frameTex = st.texture;
  c->frameView = wgpuTextureCreateView(c->frameTex, NULL);

  c->encoder = wgpuDeviceCreateCommandEncoder(c->device, NULL);
  WGPURenderPassColorAttachment colour = {
    .view = c->frameView,
    .depthSlice = WGPU_DEPTH_SLICE_UNDEFINED,
    .loadOp = WGPULoadOp_Clear,
    .storeOp = WGPUStoreOp_Store,
    .clearValue = { r, g, b, 1.0 },
  };
  WGPURenderPassDepthStencilAttachment depth = {
    .view = c->depthView,
    .depthLoadOp = WGPULoadOp_Clear,
    .depthStoreOp = WGPUStoreOp_Store,
    .depthClearValue = 1.0f,
  };
  WGPURenderPassDescriptor rp = { .colorAttachmentCount = 1,
                                  .colorAttachments = &colour,
                                  .depthStencilAttachment = &depth };
  c->pass = wgpuCommandEncoderBeginRenderPass(c->encoder, &rp);
  c->boundVerts = NULL;
  c->boundPipe = NULL;
  c->pendCount = 0;
  return 1;
}

/* One upload of the engine blocks for every draw -- material data went up
 * when the assets were made -- then the sorted transparent phase, then
 * submit. Queue writes are ordered before the submit that follows. */
void offler_end(void *p, float *objects, int floatCount) {
  Ctx *c = (Ctx *)p;
  if (!c->pass) return;
  if (floatCount > 0) {
    wgpuQueueWriteBuffer(c->queue, c->objBuf, 0, objects,
                         (size_t)floatCount * sizeof(float));
  }
  if (c->pendCount > 0) {
    qsort(c->pend, (size_t)c->pendCount, sizeof(Pending), pend_cmp);
    for (int i = 0; i < c->pendCount; i++)
      exec_draw(c, &c->pend[i], 1);
    c->pendCount = 0;
  }
  wgpuRenderPassEncoderEnd(c->pass);
  WGPUCommandBuffer cmd = wgpuCommandEncoderFinish(c->encoder, NULL);
  wgpuQueueSubmit(c->queue, 1, &cmd);
  wgpuSurfacePresent(c->surface);

  wgpuCommandBufferRelease(cmd);
  wgpuRenderPassEncoderRelease(c->pass);
  wgpuCommandEncoderRelease(c->encoder);
  wgpuTextureViewRelease(c->frameView);
  wgpuTextureRelease(c->frameTex);
  c->pass = NULL; c->encoder = NULL; c->frameView = NULL; c->frameTex = NULL;
}

void offler_quit(void *p) {
  Ctx *c = (Ctx *)p;
  if (!c) return;
  SDL_DestroyWindow(c->window);
  SDL_Quit();
  free(c->meshes);
  free(c->texs);
  for (int i = 0; i < c->matCount; i++) free(c->mats[i].bgs);
  free(c->mats);
  free(c->assets);
  free(c->pend);
  free(c);
}
