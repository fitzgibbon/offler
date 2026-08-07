/* Native backing for offler: SDL3 for the window and input, wgpu-native for
 * the drawing.
 *
 * The granularity here deliberately matches the JavaScript lambdas in
 * Offler/Web/Gpu.idr, one C function per `%foreign`. That is not a
 * coincidence: Idris cannot build nested C structs across the FFI, so
 * descriptors have to be assembled on this side -- which is exactly what a JS
 * lambda was already doing. No scene logic lives here. Matrices, transforms,
 * materials and the frame loop are all Idris; this file only creates objects
 * and records commands.
 */
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include <SDL3/SDL.h>
#include <webgpu/webgpu.h>

#ifdef __APPLE__
#include <SDL3/SDL_metal.h>
#endif

/* Nothing about the layout is defined here. Sizes, strides, the bind group
 * entries and the vertex attributes all arrive through offler_init from
 * Offler.Gfx.Layout, which is where they are derived and proved. */
#define MAX_BINDINGS 8
#define MAX_ATTRS    8

#define SV(s) ((WGPUStringView){ .data = (s), .length = strlen(s) })

/* "a,b,c;a,b,c" into up to `max` records of `n` ints. Returns the record
 * count. Runs once, at startup, on a string this program generated. */
static int parse_spec(const char *s, int n, int max, int *out) {
  int count = 0;
  while (*s && count < max) {
    for (int f = 0; f < n; f++) {
      out[count * n + f] = (int)strtol(s, (char **)&s, 10);
      if (*s == ',') s++;
    }
    if (*s == ';') s++;
    count++;
  }
  return count;
}

static WGPUVertexFormat f32_format(int n) {
  switch (n) {
    case 1:  return WGPUVertexFormat_Float32;
    case 2:  return WGPUVertexFormat_Float32x2;
    case 4:  return WGPUVertexFormat_Float32x4;
    default: return WGPUVertexFormat_Float32x3;
  }
}

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

/* ----------------------------------------------------------------- context */

typedef struct {
  WGPUBuffer buf;
  int count;
} Mesh;

typedef struct {
  SDL_Window *window;
  WGPUInstance instance;
  WGPUAdapter adapter;
  WGPUDevice device;
  WGPUQueue queue;
  WGPUSurface surface;
  WGPUTextureFormat format;
  WGPUBindGroupLayout bgl;
  WGPURenderPipeline pipeline, linePipeline;
  WGPUBuffer globalBuf, objBuf, lineBuf;
  WGPUBindGroup bindGroup;
  WGPUTexture depth;
  WGPUTextureView depthView;
  int width, height;
  int lineCount;
  /* Told to us by Idris, from Offler.Gfx.Layout -- not defined twice. */
  int globalSize, objSize, objStride, maxObjects;

  /* The mesh table a MeshHandle indexes. */
  Mesh *meshes;
  int meshCount, meshCap;

  /* Live only between offler_begin and offler_end. */
  WGPUCommandEncoder encoder;
  WGPURenderPassEncoder pass;
  WGPUTexture frameTex;
  WGPUTextureView frameView;
  WGPUBuffer boundVerts;

  /* The event being reported, refreshed by each offler_poll. */
  char eventKey[32];
  double eventX, eventY, eventWheel;
  int eventButton;
} Ctx;

/* Adapter and device arrive by callback even in C. There is nothing to do
 * while waiting, so spin rather than inflict a continuation on Idris the way
 * the browser's promise-based API forces. */
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

void *offler_init(const char *title, const char *wgsl,
                  const char *bindSpec, const char *meshSpec,
                  const char *lineSpec,
                  int meshStride, int lineStride, int objStride,
                  int maxObjects) {
  Ctx *c = (Ctx *)calloc(1, sizeof(Ctx));
  c->objStride = objStride;
  c->maxObjects = maxObjects;
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

  /* Shader, layouts and pipelines: one nested descriptor each, assembled
   * here for the same reason prim__pipeline assembles them inside a JS
   * lambda. */
  WGPUShaderSourceWGSL src = {
    .chain = { .sType = WGPUSType_ShaderSourceWGSL },
    .code = SV(wgsl),
  };
  WGPUShaderModuleDescriptor smd = { .nextInChain = &src.chain };
  WGPUShaderModule module = wgpuDeviceCreateShaderModule(c->device, &smd);

  /* binding, visibility, hasDynamicOffset, minBindingSize -- one record per
   * uniform block, in the order Offler.Gfx.Layout declares them. */
  int bs[MAX_BINDINGS * 4];
  int nb = parse_spec(bindSpec, 4, MAX_BINDINGS, bs);
  WGPUBindGroupLayoutEntry entries[MAX_BINDINGS] = {0};
  for (int i = 0; i < nb; i++) {
    entries[i].binding = (uint32_t)bs[i * 4 + 0];
    entries[i].visibility = (WGPUShaderStage)bs[i * 4 + 1];
    entries[i].buffer.type = WGPUBufferBindingType_Uniform;
    entries[i].buffer.hasDynamicOffset = bs[i * 4 + 2] != 0;
    entries[i].buffer.minBindingSize = (uint64_t)bs[i * 4 + 3];
  }
  c->globalSize = bs[0 * 4 + 3];
  c->objSize = bs[1 * 4 + 3];

  WGPUBindGroupLayoutDescriptor bgld = { .entryCount = (size_t)nb,
                                         .entries = entries };
  c->bgl = wgpuDeviceCreateBindGroupLayout(c->device, &bgld);

  WGPUPipelineLayoutDescriptor pld = { .bindGroupLayoutCount = 1,
                                       .bindGroupLayouts = &c->bgl };
  WGPUPipelineLayout layout = wgpuDeviceCreatePipelineLayout(c->device, &pld);

  /* location, byte offset, float32 components -- once per pipeline, since
   * the mesh and the line vertex carry different attributes. */
  int mv[MAX_ATTRS * 3];
  int nma = parse_spec(meshSpec, 3, MAX_ATTRS, mv);
  WGPUVertexAttribute meshAttrs[MAX_ATTRS] = {0};
  for (int i = 0; i < nma; i++) {
    meshAttrs[i].shaderLocation = (uint32_t)mv[i * 3 + 0];
    meshAttrs[i].offset = (uint64_t)mv[i * 3 + 1];
    meshAttrs[i].format = f32_format(mv[i * 3 + 2]);
  }
  int lv[MAX_ATTRS * 3];
  int nla = parse_spec(lineSpec, 3, MAX_ATTRS, lv);
  WGPUVertexAttribute lineAttrs[MAX_ATTRS] = {0};
  for (int i = 0; i < nla; i++) {
    lineAttrs[i].shaderLocation = (uint32_t)lv[i * 3 + 0];
    lineAttrs[i].offset = (uint64_t)lv[i * 3 + 1];
    lineAttrs[i].format = f32_format(lv[i * 3 + 2]);
  }

  WGPUVertexBufferLayout vbl = { .stepMode = WGPUVertexStepMode_Vertex,
                                 .arrayStride = (uint64_t)meshStride,
                                 .attributeCount = (size_t)nma,
                                 .attributes = meshAttrs };
  WGPUColorTargetState target = { .format = c->format,
                                  .writeMask = WGPUColorWriteMask_All };
  WGPUFragmentState frag = { .module = module, .entryPoint = SV("fs"),
                             .targetCount = 1, .targets = &target };
  WGPUDepthStencilState depth = {
    .format = WGPUTextureFormat_Depth24Plus,
    .depthWriteEnabled = WGPUOptionalBool_True,
    .depthCompare = WGPUCompareFunction_Less,
    .stencilFront = { .compare = WGPUCompareFunction_Always },
    .stencilBack  = { .compare = WGPUCompareFunction_Always },
  };
  WGPURenderPipelineDescriptor rpd = {
    .layout = layout,
    .vertex = { .module = module, .entryPoint = SV("vs"),
                .bufferCount = 1, .buffers = &vbl },
    .primitive = { .topology = WGPUPrimitiveTopology_TriangleList,
                   .frontFace = WGPUFrontFace_CCW,
                   .cullMode = WGPUCullMode_Back },
    .depthStencil = &depth,
    .multisample = { .count = 1, .mask = 0xFFFFFFFF },
    .fragment = &frag,
  };
  c->pipeline = wgpuDeviceCreateRenderPipeline(c->device, &rpd);

  /* The overlay: same module and layout, but the vs_line entry point, line
   * topology, blending, and no depth writes so lines do not stipple each
   * other where they cross. Line width is not a pipeline setting in WebGPU --
   * lines are always one pixel, which is what is wanted. */
  WGPUVertexBufferLayout lvbl = { .stepMode = WGPUVertexStepMode_Vertex,
                                  .arrayStride = (uint64_t)lineStride,
                                  .attributeCount = (size_t)nla,
                                  .attributes = lineAttrs };
  WGPUBlendState blend = {
    .color = { .operation = WGPUBlendOperation_Add,
               .srcFactor = WGPUBlendFactor_SrcAlpha,
               .dstFactor = WGPUBlendFactor_OneMinusSrcAlpha },
    .alpha = { .operation = WGPUBlendOperation_Add,
               .srcFactor = WGPUBlendFactor_One,
               .dstFactor = WGPUBlendFactor_OneMinusSrcAlpha },
  };
  WGPUColorTargetState lineTarget = { .format = c->format, .blend = &blend,
                                      .writeMask = WGPUColorWriteMask_All };
  WGPUFragmentState lineFrag = { .module = module, .entryPoint = SV("fs"),
                                 .targetCount = 1, .targets = &lineTarget };
  WGPUDepthStencilState lineDepth = depth;
  lineDepth.depthWriteEnabled = WGPUOptionalBool_False;
  WGPURenderPipelineDescriptor lrpd = {
    .layout = layout,
    .vertex = { .module = module, .entryPoint = SV("vs_line"),
                .bufferCount = 1, .buffers = &lvbl },
    .primitive = { .topology = WGPUPrimitiveTopology_LineList,
                   .frontFace = WGPUFrontFace_CCW,
                   .cullMode = WGPUCullMode_None },
    .depthStencil = &lineDepth,
    .multisample = { .count = 1, .mask = 0xFFFFFFFF },
    .fragment = &lineFrag,
  };
  c->linePipeline = wgpuDeviceCreateRenderPipeline(c->device, &lrpd);

  WGPUBufferDescriptor gbd = { .usage = WGPUBufferUsage_Uniform | WGPUBufferUsage_CopyDst,
                               .size = (uint64_t)c->globalSize };
  c->globalBuf = wgpuDeviceCreateBuffer(c->device, &gbd);
  WGPUBufferDescriptor obd = { .usage = WGPUBufferUsage_Uniform | WGPUBufferUsage_CopyDst,
                               .size = (uint64_t)objStride * maxObjects };
  c->objBuf = wgpuDeviceCreateBuffer(c->device, &obd);

  WGPUBindGroupEntry bge[2] = {
    { .binding = 0, .buffer = c->globalBuf, .size = (uint64_t)c->globalSize },
    { .binding = 1, .buffer = c->objBuf, .size = (uint64_t)c->objSize },
  };
  WGPUBindGroupDescriptor bgd = { .layout = c->bgl, .entryCount = 2, .entries = bge };
  c->bindGroup = wgpuDeviceCreateBindGroup(c->device, &bgd);

  c->meshCap = 16;
  c->meshes = (Mesh *)calloc((size_t)c->meshCap, sizeof(Mesh));

  return c;
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
  /* Single letters arrive uppercase from SDL and lowercase from the DOM. */
  if (c->eventKey[0] && !c->eventKey[1]
      && c->eventKey[0] >= 'A' && c->eventKey[0] <= 'Z')
    c->eventKey[0] += 'a' - 'A';
}

/* Window coordinates to drawing-surface pixels, matching the browser's
 * devicePixelRatio scaling. */
static void set_pointer(Ctx *c, float x, float y) {
  int ww = 1, wh = 1;
  SDL_GetWindowSize(c->window, &ww, &wh);
  if (ww < 1) ww = 1;
  if (wh < 1) wh = 1;
  c->eventX = (double)x * c->width / ww;
  c->eventY = (double)y * c->height / wh;
}

/* One event per call, 0 when the queue is empty, so Idris drains it in a
 * loop exactly as it drains the browser's queue.
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
        set_pointer(c, e.button.x, e.button.y);
        c->eventButton = e.button.button == SDL_BUTTON_LEFT ? 0
                       : e.button.button == SDL_BUTTON_MIDDLE ? 1
                       : e.button.button == SDL_BUTTON_RIGHT ? 2
                       : (int)e.button.button;
        return 6;
      case SDL_EVENT_MOUSE_BUTTON_UP:
        set_pointer(c, e.button.x, e.button.y);
        c->eventButton = e.button.button == SDL_BUTTON_LEFT ? 0
                       : e.button.button == SDL_BUTTON_MIDDLE ? 1
                       : e.button.button == SDL_BUTTON_RIGHT ? 2
                       : (int)e.button.button;
        return 7;
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

/* ----------------------------------------------------------------- drawing */

int offler_create_mesh(void *p, float *verts, int floatCount, int vertexCount) {
  Ctx *c = (Ctx *)p;
  if (c->meshCount == c->meshCap) {
    c->meshCap *= 2;
    c->meshes = (Mesh *)realloc(c->meshes, (size_t)c->meshCap * sizeof(Mesh));
  }
  size_t bytes = (size_t)floatCount * sizeof(float);
  WGPUBufferDescriptor bd = { .usage = WGPUBufferUsage_Vertex | WGPUBufferUsage_CopyDst,
                              .size = bytes };
  WGPUBuffer buf = wgpuDeviceCreateBuffer(c->device, &bd);
  wgpuQueueWriteBuffer(c->queue, buf, 0, verts, bytes);
  c->meshes[c->meshCount] = (Mesh){ .buf = buf, .count = vertexCount };
  return c->meshCount++;
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

/* Returns 0 if the frame cannot be started, so Idris can skip it. */
int offler_begin(void *p, float *globals, double r, double g, double b) {
  Ctx *c = (Ctx *)p;

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
  wgpuRenderPassEncoderSetPipeline(c->pass, c->pipeline);
  c->boundVerts = NULL;
  return 1;
}

static void bind_mesh(Ctx *c, int mesh) {
  WGPUBuffer buf = c->meshes[mesh].buf;
  if (buf != c->boundVerts) {
    wgpuRenderPassEncoderSetVertexBuffer(c->pass, 0, buf, 0, WGPU_WHOLE_SIZE);
    c->boundVerts = buf;
  }
}

void offler_draw(void *p, int mesh, int index) {
  Ctx *c = (Ctx *)p;
  if (!c->pass || mesh < 0 || mesh >= c->meshCount || index >= c->maxObjects) return;
  bind_mesh(c, mesh);
  uint32_t offset = (uint32_t)index * c->objStride;
  wgpuRenderPassEncoderSetBindGroup(c->pass, 0, c->bindGroup, 1, &offset);
  wgpuRenderPassEncoderDraw(c->pass, (uint32_t)c->meshes[mesh].count, 1, 0, 0);
}

/* The batched form: the slots from `first` for `count` are already filled,
 * so record their draws in one call from Idris rather than one each. */
void offler_draw_slices(void *p, int mesh, int first, int count) {
  Ctx *c = (Ctx *)p;
  if (!c->pass || mesh < 0 || mesh >= c->meshCount) return;
  bind_mesh(c, mesh);
  uint32_t n = (uint32_t)c->meshes[mesh].count;
  for (int i = 0; i < count; i++) {
    if (first + i >= c->maxObjects) return;
    uint32_t offset = (uint32_t)(first + i) * c->objStride;
    wgpuRenderPassEncoderSetBindGroup(c->pass, 0, c->bindGroup, 1, &offset);
    wgpuRenderPassEncoderDraw(c->pass, n, 1, 0, 0);
  }
}

/* Swap in the line pipeline for one draw, then put the mesh pipeline back so
 * what follows records against the state it expects. */
void offler_draw_lines(void *p, int index) {
  Ctx *c = (Ctx *)p;
  if (!c->pass || !c->lineBuf || c->lineCount <= 0 || index >= c->maxObjects) return;
  uint32_t offset = (uint32_t)index * c->objStride;
  wgpuRenderPassEncoderSetPipeline(c->pass, c->linePipeline);
  wgpuRenderPassEncoderSetVertexBuffer(c->pass, 0, c->lineBuf, 0, WGPU_WHOLE_SIZE);
  wgpuRenderPassEncoderSetBindGroup(c->pass, 0, c->bindGroup, 1, &offset);
  wgpuRenderPassEncoderDraw(c->pass, (uint32_t)c->lineCount, 1, 0, 0);
  wgpuRenderPassEncoderSetPipeline(c->pass, c->pipeline);
  c->boundVerts = NULL;
}

/* One upload for every object, as in the browser: writing per object means a
 * queue call per draw. Queue writes are ordered before the submit that
 * follows. */
void offler_end(void *p, float *objects, int floatCount) {
  Ctx *c = (Ctx *)p;
  if (!c->pass) return;
  if (floatCount > 0)
    wgpuQueueWriteBuffer(c->queue, c->objBuf, 0, objects,
                         (size_t)floatCount * sizeof(float));
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
  free(c);
}
