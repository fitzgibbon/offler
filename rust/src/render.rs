//! The wgpu half of the context: the same retained material assets, paged
//! object and material buffers, cached bind groups, pipeline variants and
//! sorted transparent phase as the C shim's `webgpu.h` code.

use std::sync::Arc;

use winit::window::Window;

use crate::array::{F32Array, U32Array, count};
use crate::spec::{self, VertexAttr};

/// What Idris tells the renderer about the layout, from `Offler.Gfx.Layout`.
pub struct Layout {
    pub global_size: u64,
    pub obj_size: u64,
    pub obj_stride: u64,
    pub max_objects: i64,
    pub mesh_stride: u64,
    pub line_stride: u64,
    pub inst_stride: u64,
    pub mesh_attrs: Vec<VertexAttr>,
    pub line_attrs: Vec<VertexAttr>,
    pub inst_attrs: Vec<VertexAttr>,
}

struct Mesh {
    buf: Option<wgpu::Buffer>,
    count: u32,
    lines: bool,
    ibuf: Option<wgpu::Buffer>,
    icount: u32,
    /// Bumped at free; draws with a stale generation skip.
    generation: i32,
}

/// A material's pipeline variants.
struct Mat {
    tri_opaque: wgpu::RenderPipeline,
    tri_blend: wgpu::RenderPipeline,
    line: Option<(wgpu::RenderPipeline, wgpu::RenderPipeline)>,
    inst: Option<wgpu::RenderPipeline>,
    bgl: wgpu::BindGroupLayout,
    tex_count: usize,
    mat_size: u64,
}

/// Which pipeline a pass has bound, to skip redundant binds.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Pipe {
    Tri { mat: usize, blend: bool },
    Line { mat: usize, blend: bool },
}

/// A material asset: a slot of the paged material buffers, its textures,
/// and a bind group per object page.
struct Asset {
    mat: usize,
    slot: i64,
    tex: [i64; 4],
    bgs: Vec<Option<wgpu::BindGroup>>,
}

#[derive(Default)]
struct Instances {
    buf: Option<wgpu::Buffer>,
    cap: u64,
    n: i32,
}

#[derive(Clone, Copy)]
struct Pending {
    asset: usize,
    mesh: usize,
    generation: i32,
    slot: i64,
    depth: f64,
}

/// Live only between `offler_begin` and `offler_end`. The pass is declared
/// first so it is dropped before the encoder it records into.
struct Frame {
    pass: wgpu::RenderPass<'static>,
    encoder: wgpu::CommandEncoder,
    texture: wgpu::SurfaceTexture,
    bound_verts: Option<(usize, i32)>,
    bound_pipe: Option<Pipe>,
    pending: Vec<Pending>,
}

/// Fields drop in order: the frame before the surface, and the window,
/// which the surface draws to, last.
pub struct Gpu {
    frame: Option<Frame>,
    surface: wgpu::Surface<'static>,
    device: wgpu::Device,
    queue: wgpu::Queue,
    format: wgpu::TextureFormat,
    pub width: u32,
    pub height: u32,
    layout: Layout,
    global_buf: wgpu::Buffer,
    line_buf: Option<wgpu::Buffer>,
    line_count: i64,
    obj_pages: Vec<wgpu::Buffer>,
    mat_pages: Vec<wgpu::Buffer>,
    depth_view: Option<wgpu::TextureView>,
    sampler: wgpu::Sampler,
    white: wgpu::TextureView,
    line_pipeline: wgpu::RenderPipeline,
    line_bgl: wgpu::BindGroupLayout,
    line_bgs: Vec<Option<wgpu::BindGroup>>,
    insts: Vec<Instances>,
    assets: Vec<Asset>,
    meshes: Vec<Mesh>,
    free_meshes: Vec<usize>,
    texs: Vec<wgpu::TextureView>,
    mats: Vec<Mat>,
    window: Arc<Window>,
}

const DEPTH_FORMAT: wgpu::TextureFormat = wgpu::TextureFormat::Depth24Plus;

fn bytes(fs: &[f32]) -> &[u8] {
    bytemuck::cast_slice(fs)
}

fn idx(i: i64, len: usize) -> Option<usize> {
    usize::try_from(i).ok().filter(|&i| i < len)
}

/// The C's `int` arithmetic for a dynamic offset.
fn offset(slot: i64, max_objects: i64, stride: u64) -> u32 {
    ((slot % max_objects) as u64 * stride) as u32
}

fn page(slot: i64, max_objects: i64) -> usize {
    (slot / max_objects) as usize
}

impl Gpu {
    pub fn new(
        window: Arc<Window>,
        layout: Layout,
        line_wgsl: &str,
        line_bind_spec: &str,
    ) -> Result<Self, String> {
        let instance = wgpu::Instance::new(wgpu::InstanceDescriptor::new_with_display_handle(
            Box::new(window.clone()),
        ));
        let surface = instance
            .create_surface(window.clone())
            .map_err(|e| format!("no surface: {e}"))?;
        let adapter = pollster::block_on(instance.request_adapter(&wgpu::RequestAdapterOptions {
            compatible_surface: Some(&surface),
            ..Default::default()
        }))
        .map_err(|e| format!("no adapter: {e}"))?;
        let (device, queue) =
            pollster::block_on(adapter.request_device(&wgpu::DeviceDescriptor::default()))
                .map_err(|e| format!("no device: {e}"))?;
        let format = surface
            .get_capabilities(&adapter)
            .formats
            .first()
            .copied()
            .unwrap_or(wgpu::TextureFormat::Bgra8Unorm);

        let global_buf = device.create_buffer(&wgpu::BufferDescriptor {
            label: None,
            size: layout.global_size,
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            address_mode_u: wgpu::AddressMode::Repeat,
            address_mode_v: wgpu::AddressMode::Repeat,
            address_mode_w: wgpu::AddressMode::Repeat,
            mag_filter: wgpu::FilterMode::Linear,
            min_filter: wgpu::FilterMode::Linear,
            mipmap_filter: wgpu::MipmapFilterMode::Nearest,
            ..Default::default()
        });
        // The 1x1 white every unmapped texture slot binds.
        let white = upload_texture(
            &device,
            &queue,
            &[255; 4],
            1,
            1,
            wgpu::TextureFormat::Rgba8Unorm,
        );

        // The engine's line pipeline: generated WGSL against the engine
        // blocks, whose lane carries the colour.
        let module = shader(&device, line_wgsl);
        let bind = spec::parse_bind_spec(line_bind_spec);
        let line_bgl = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: None,
            entries: &spec::bind_entries(&bind),
        });
        let pl = pipeline_layout(&device, &line_bgl);
        let line_pipeline = make_pipeline(
            &device,
            format,
            &module,
            &pl,
            PipelineKind::Plain {
                entry: "vs",
                attrs: &layout.line_attrs,
                stride: layout.line_stride,
                lines: true,
                blend: true,
            },
        );

        let mut gpu = Self {
            window,
            surface,
            device,
            queue,
            format,
            width: 1,
            height: 1,
            layout,
            global_buf,
            line_buf: None,
            line_count: 0,
            obj_pages: Vec::new(),
            mat_pages: Vec::new(),
            depth_view: None,
            sampler,
            white,
            line_pipeline,
            line_bgl,
            line_bgs: Vec::new(),
            insts: Vec::new(),
            assets: Vec::new(),
            meshes: Vec::new(),
            free_meshes: Vec::new(),
            texs: Vec::new(),
            mats: Vec::new(),
            frame: None,
        };
        gpu.configure();
        Ok(gpu)
    }

    /// Size the surface to the window, and rebuild the depth attachment
    /// with it: it does not follow the surface on its own.
    pub fn configure(&mut self) {
        let s = self.window.inner_size();
        self.width = s.width.max(1);
        self.height = s.height.max(1);
        let cfg = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: self.format,
            color_space: Default::default(),
            width: self.width,
            height: self.height,
            present_mode: wgpu::PresentMode::Fifo,
            desired_maximum_frame_latency: 2,
            alpha_mode: wgpu::CompositeAlphaMode::Auto,
            view_formats: Vec::new(),
        };
        self.surface.configure(&self.device, &cfg);
        let depth = self.device.create_texture(&wgpu::TextureDescriptor {
            label: None,
            size: wgpu::Extent3d {
                width: self.width,
                height: self.height,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: DEPTH_FORMAT,
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            view_formats: &[],
        });
        self.depth_view = Some(depth.create_view(&Default::default()));
    }

    pub fn window(&self) -> &Window {
        &self.window
    }

    // ------------------------------------------------------------ materials

    pub fn register_material(
        &mut self,
        wgsl: &str,
        bind_spec: &str,
        has_line: bool,
        has_inst: bool,
    ) -> i64 {
        let module = shader(&self.device, wgsl);
        let bind = spec::parse_bind_spec(bind_spec);
        let bgl = self
            .device
            .create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: None,
                entries: &spec::bind_entries(&bind),
            });
        let pl = pipeline_layout(&self.device, &bgl);
        let l = &self.layout;
        let tri = |blend| PipelineKind::Plain {
            entry: "vs",
            attrs: &l.mesh_attrs,
            stride: l.mesh_stride,
            lines: false,
            blend,
        };
        let line = |blend| PipelineKind::Plain {
            entry: "vs_line",
            attrs: &l.line_attrs,
            stride: l.line_stride,
            lines: true,
            blend,
        };
        let mk = |k| make_pipeline(&self.device, self.format, &module, &pl, k);
        let mat = Mat {
            tri_opaque: mk(tri(false)),
            tri_blend: mk(tri(true)),
            line: has_line.then(|| (mk(line(false)), mk(line(true)))),
            inst: has_inst.then(|| {
                mk(PipelineKind::Instanced {
                    mesh_attrs: &l.mesh_attrs,
                    mesh_stride: l.mesh_stride,
                    inst_attrs: &l.inst_attrs,
                    inst_stride: l.inst_stride,
                })
            }),
            bgl,
            tex_count: bind.tex_count,
            mat_size: bind.mat_size,
        };
        self.mats.push(mat);
        self.mats.len() as i64 - 1
    }

    // ------------------------------------------------------------- textures

    pub fn add_texture(&mut self, rgba: &[u8], w: u32, h: u32) -> i64 {
        let view = upload_texture(
            &self.device,
            &self.queue,
            rgba,
            w,
            h,
            wgpu::TextureFormat::Rgba8UnormSrgb,
        );
        self.texs.push(view);
        self.texs.len() as i64 - 1
    }

    /// A decode failure still yields a valid handle, bound to the white.
    pub fn white_entry(&mut self) -> i64 {
        self.texs.push(self.white.clone());
        self.texs.len() as i64 - 1
    }

    // ---------------------------------------------------------------- pages

    fn page_at(&mut self, objects: bool, p: usize) -> wgpu::Buffer {
        let size = self.layout.obj_stride * self.layout.max_objects as u64;
        let pages = if objects {
            &mut self.obj_pages
        } else {
            &mut self.mat_pages
        };
        while pages.len() <= p {
            pages.push(self.device.create_buffer(&wgpu::BufferDescriptor {
                label: None,
                size,
                usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
                mapped_at_creation: false,
            }));
        }
        pages[p].clone()
    }

    pub fn upload_obj_page(&mut self, page: i64, data: &F32Array, float_count: i64) {
        if float_count <= 0 {
            return;
        }
        let Ok(p) = usize::try_from(page) else { return };
        let buf = self.page_at(true, p);
        self.queue
            .write_buffer(&buf, 0, bytes(&data.prefix(count(float_count))));
    }

    /// One asset's slot, uploaded when the asset is made or updated:
    /// `local` is the staging slot, `global` the slot across the pages.
    pub fn upload_mat_slot(&mut self, data: &F32Array, local: i64, global: i64) {
        let l = &self.layout;
        let (Ok(local), Ok(_)) = (usize::try_from(local), usize::try_from(global)) else {
            return;
        };
        let floats = (l.obj_stride / 4) as usize;
        let at = (global % l.max_objects) as u64 * l.obj_stride;
        let buf = self.page_at(false, page(global, self.layout.max_objects));
        let d = data.borrow();
        if let Some(run) = d.get(local * floats..(local + 1) * floats) {
            self.queue.write_buffer(&buf, at, bytes(run));
        }
    }

    // --------------------------------------------------------------- meshes

    /// Recycled indices first; a recycled entry keeps its bumped generation.
    fn push_mesh(
        &mut self,
        buf: wgpu::Buffer,
        count: u32,
        lines: bool,
        index: Option<(wgpu::Buffer, u32)>,
    ) -> i64 {
        let (ibuf, icount) = index.map_or((None, 0), |(b, n)| (Some(b), n));
        let i = match self.free_meshes.pop() {
            Some(i) => i,
            None => {
                self.meshes.push(Mesh {
                    buf: None,
                    count: 0,
                    lines: false,
                    ibuf: None,
                    icount: 0,
                    generation: 0,
                });
                self.meshes.len() - 1
            }
        };
        let m = &mut self.meshes[i];
        (m.buf, m.count, m.lines, m.ibuf, m.icount) = (Some(buf), count, lines, ibuf, icount);
        i as i64
    }

    pub fn mesh_gen(&self, i: i64) -> i64 {
        idx(i, self.meshes.len()).map_or(0, |i| i64::from(self.meshes[i].generation))
    }

    fn upload_static(&self, data: &[u8], usage: wgpu::BufferUsages) -> wgpu::Buffer {
        let buf = self.device.create_buffer(&wgpu::BufferDescriptor {
            label: None,
            size: data.len() as u64,
            usage: usage | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        if !data.is_empty() {
            self.queue.write_buffer(&buf, 0, data);
        }
        buf
    }

    pub fn create_mesh(&mut self, verts: &F32Array, vertex_count: i64, topo: i64) -> i64 {
        let lines = topo as i32 == 1;
        let stride = if lines {
            self.layout.line_stride
        } else {
            self.layout.mesh_stride
        };
        let floats = vertex_count * stride as i64 / 4;
        let buf = self.upload_static(
            bytes(&verts.prefix(count(floats))),
            wgpu::BufferUsages::VERTEX,
        );
        self.push_mesh(buf, vertex_count as u32, lines, None)
    }

    pub fn create_mesh_indexed(
        &mut self,
        verts: &F32Array,
        vertex_count: i64,
        idx: &U32Array,
        idx_count: i64,
    ) -> i64 {
        let floats = vertex_count * self.layout.mesh_stride as i64 / 4;
        let buf = self.upload_static(
            bytes(&verts.prefix(count(floats))),
            wgpu::BufferUsages::VERTEX,
        );
        let ib = self.upload_static(
            bytemuck::cast_slice(&idx.prefix(count(idx_count))),
            wgpu::BufferUsages::INDEX,
        );
        self.push_mesh(
            buf,
            vertex_count as u32,
            false,
            Some((ib, idx_count as u32)),
        )
    }

    /// Release the buffers, bump the generation and recycle the index. A
    /// stale free is a no-op.
    pub fn free_mesh(&mut self, mi: i64, generation: i64) {
        let Some(i) = idx(mi, self.meshes.len()) else {
            return;
        };
        let m = &mut self.meshes[i];
        if m.buf.is_none() || m.generation != generation as i32 {
            return;
        }
        (m.buf, m.ibuf, m.count, m.icount) = (None, None, 0, 0);
        m.generation = m.generation.wrapping_add(1);
        self.free_meshes.push(i);
    }

    pub fn set_lines(&mut self, verts: &F32Array, float_count: i64, vertex_count: i64) {
        self.line_buf = None;
        self.line_count = vertex_count;
        if vertex_count <= 0 {
            return;
        }
        let data = verts.prefix(count(float_count));
        self.line_buf = Some(self.upload_static(bytes(&data), wgpu::BufferUsages::VERTEX));
    }

    // --------------------------------------------------------------- assets

    pub fn add_asset(&mut self, mat: i64, slot: i64, tex: [i64; 4]) -> i64 {
        self.assets.push(Asset {
            mat: usize::try_from(mat).unwrap_or(usize::MAX),
            slot,
            tex,
            bgs: Vec::new(),
        });
        self.assets.len() as i64 - 1
    }

    pub fn update_asset(&mut self, ai: i64, tex: [i64; 4]) {
        if let Some(i) = idx(ai, self.assets.len()) {
            let a = &mut self.assets[i];
            a.tex = tex;
            a.bgs.clear();
        }
    }

    /// The bind group for an asset and an object page, made on first use.
    fn asset_bg(&mut self, ai: usize, obj_page: usize) -> Option<wgpu::BindGroup> {
        if let Some(Some(bg)) = self.assets[ai].bgs.get(obj_page) {
            return Some(bg.clone());
        }
        let a = &self.assets[ai];
        let m = self.mats.get(a.mat)?;
        let (bgl, tex_count, mat_size, slot, tex) =
            (m.bgl.clone(), m.tex_count, m.mat_size, a.slot, a.tex);
        let obj = self.page_at(true, obj_page);
        let mat = self.page_at(false, page(slot, self.layout.max_objects));
        let views: Vec<wgpu::TextureView> = (0..tex_count)
            .map(|i| {
                let t = tex.get(i).copied().unwrap_or(-1);
                idx(t, self.texs.len()).map_or_else(|| self.white.clone(), |t| self.texs[t].clone())
            })
            .collect();
        let mut entries = vec![
            buffer_entry(0, &self.global_buf, self.layout.global_size),
            buffer_entry(1, &obj, self.layout.obj_size),
            buffer_entry(2, &mat, mat_size),
        ];
        for (i, v) in views.iter().enumerate() {
            let i = i as u32;
            entries.push(wgpu::BindGroupEntry {
                binding: 3 + 2 * i,
                resource: wgpu::BindingResource::TextureView(v),
            });
            entries.push(wgpu::BindGroupEntry {
                binding: 4 + 2 * i,
                resource: wgpu::BindingResource::Sampler(&self.sampler),
            });
        }
        let bg = self.device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: None,
            layout: &bgl,
            entries: &entries,
        });
        let bgs = &mut self.assets[ai].bgs;
        if bgs.len() <= obj_page {
            bgs.resize(obj_page + 4, None);
        }
        bgs[obj_page] = Some(bg.clone());
        Some(bg)
    }

    fn line_bg(&mut self, p: usize) -> wgpu::BindGroup {
        if let Some(Some(bg)) = self.line_bgs.get(p) {
            return bg.clone();
        }
        let obj = self.page_at(true, p);
        let bg = self.device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: None,
            layout: &self.line_bgl,
            entries: &[
                buffer_entry(0, &self.global_buf, self.layout.global_size),
                buffer_entry(1, &obj, self.layout.obj_size),
            ],
        });
        if self.line_bgs.len() <= p {
            self.line_bgs.resize(p + 4, None);
        }
        self.line_bgs[p] = Some(bg.clone());
        bg
    }

    // -------------------------------------------------------------- drawing

    fn exec_draw(&mut self, d: Pending, blend: bool) {
        let a = &self.assets[d.asset];
        let Some(m) = self.mats.get(a.mat) else {
            return;
        };
        let mm = &self.meshes[d.mesh];
        let Some(vb) = mm.buf.clone() else { return };
        if mm.generation != d.generation {
            return;
        }
        let (key, pipe) = if mm.lines {
            match &m.line {
                Some((o, b)) => (Pipe::Line { mat: a.mat, blend }, if blend { b } else { o }),
                None => return,
            }
        } else {
            (
                Pipe::Tri { mat: a.mat, blend },
                if blend { &m.tri_blend } else { &m.tri_opaque },
            )
        };
        let pipe = pipe.clone();
        let (ib, icount, vcount) = (mm.ibuf.clone(), mm.icount, mm.count);
        let l = &self.layout;
        let offs = [
            offset(d.slot, l.max_objects, l.obj_stride),
            offset(a.slot, l.max_objects, l.obj_stride),
        ];
        let Some(bg) = self.asset_bg(d.asset, page(d.slot, self.layout.max_objects)) else {
            return;
        };
        let Some(f) = self.frame.as_mut() else { return };
        if f.bound_pipe != Some(key) {
            f.pass.set_pipeline(&pipe);
            f.bound_pipe = Some(key);
        }
        if f.bound_verts != Some((d.mesh, d.generation)) {
            f.pass.set_vertex_buffer(0, vb.slice(..));
            if let Some(ib) = &ib {
                f.pass
                    .set_index_buffer(ib.slice(..), wgpu::IndexFormat::Uint32);
            }
            f.bound_verts = Some((d.mesh, d.generation));
        }
        f.pass.set_bind_group(0, &bg, &offs);
        if ib.is_some() {
            f.pass.draw_indexed(0..icount, 0, 0..1);
        } else {
            f.pass.draw(0..vcount, 0..1);
        }
    }

    fn check_draw(&self, asset: i64, mesh: i64, first: i64) -> Option<(usize, usize)> {
        if self.frame.is_none() || first < 0 {
            return None;
        }
        Some((
            idx(asset, self.assets.len())?,
            idx(mesh, self.meshes.len())?,
        ))
    }

    fn submit_draw(&mut self, d: Pending, blend: bool) {
        if blend {
            if let Some(f) = self.frame.as_mut() {
                f.pending.push(d);
            }
        } else {
            self.exec_draw(d, false);
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn draw_slices(
        &mut self,
        asset: i64,
        mesh: i64,
        generation: i64,
        first: i64,
        n: i64,
        blend: bool,
        depth: f64,
    ) {
        let Some((asset, mesh)) = self.check_draw(asset, mesh, first) else {
            return;
        };
        for i in 0..n.max(0) {
            let d = Pending {
                asset,
                mesh,
                generation: generation as i32,
                slot: first + i,
                depth,
            };
            self.submit_draw(d, blend);
        }
    }

    pub fn create_instances(&mut self) -> i64 {
        self.insts.push(Instances::default());
        self.insts.len() as i64 - 1
    }

    /// Replace an instance buffer's contents, growing it when outgrown.
    pub fn write_instances(&mut self, ih: i64, data: &F32Array, float_count: i64, n: i64) {
        let Some(i) = idx(ih, self.insts.len()) else {
            return;
        };
        let len = count(float_count) as u64 * 4;
        let inst = &mut self.insts[i];
        if inst.buf.is_none() || inst.cap < len {
            inst.cap = len.max(inst.cap * 2).max(1024);
            inst.buf = Some(self.device.create_buffer(&wgpu::BufferDescriptor {
                label: None,
                size: inst.cap,
                usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
                mapped_at_creation: false,
            }));
        }
        if len > 0
            && let Some(buf) = &inst.buf
        {
            self.queue
                .write_buffer(buf, 0, bytes(&data.prefix(len as usize / 4)));
        }
        inst.n = n as i32;
    }

    pub fn draw_instanced(&mut self, asset: i64, mesh: i64, generation: i64, slot: i64, ih: i64) {
        let Some((ai, mi)) = self.check_draw(asset, mesh, slot) else {
            return;
        };
        let Some(ii) = idx(ih, self.insts.len()) else {
            return;
        };
        let a = &self.assets[ai];
        let Some(pipe) = self.mats.get(a.mat).and_then(|m| m.inst.clone()) else {
            return;
        };
        let mm = &self.meshes[mi];
        let inst = &self.insts[ii];
        let (Some(vb), Some(ibuf)) = (mm.buf.clone(), inst.buf.clone()) else {
            return;
        };
        if mm.generation != generation as i32 || inst.n <= 0 {
            return;
        }
        let (ib, icount, vcount, n) = (mm.ibuf.clone(), mm.icount, mm.count, inst.n as u32);
        let l = &self.layout;
        let offs = [
            offset(slot, l.max_objects, l.obj_stride),
            offset(a.slot, l.max_objects, l.obj_stride),
        ];
        let Some(bg) = self.asset_bg(ai, page(slot, self.layout.max_objects)) else {
            return;
        };
        let Some(f) = self.frame.as_mut() else { return };
        f.pass.set_pipeline(&pipe);
        f.pass.set_vertex_buffer(0, vb.slice(..));
        f.pass.set_vertex_buffer(1, ibuf.slice(..));
        f.pass.set_bind_group(0, &bg, &offs);
        match ib {
            Some(ib) => {
                f.pass
                    .set_index_buffer(ib.slice(..), wgpu::IndexFormat::Uint32);
                f.pass.draw_indexed(0..icount, 0, 0..n);
            }
            None => f.pass.draw(0..vcount, 0..n),
        }
        f.bound_pipe = None;
        f.bound_verts = None;
    }

    pub fn draw_lines(&mut self, slot: i64) {
        if self.frame.is_none() || self.line_count <= 0 || slot < 0 {
            return;
        }
        let Some(lb) = self.line_buf.clone() else {
            return;
        };
        let off = offset(slot, self.layout.max_objects, self.layout.obj_stride);
        let bg = self.line_bg(page(slot, self.layout.max_objects));
        let (pipe, n) = (self.line_pipeline.clone(), self.line_count as u32);
        let Some(f) = self.frame.as_mut() else { return };
        f.pass.set_pipeline(&pipe);
        f.pass.set_vertex_buffer(0, lb.slice(..));
        f.pass.set_bind_group(0, &bg, &[off]);
        f.pass.draw(0..n, 0..1);
        f.bound_pipe = None;
        f.bound_verts = None;
    }

    /// Start a frame; false when the surface cannot give one, so Idris
    /// skips it. Resizes are absorbed here.
    pub fn begin(&mut self, globals: &F32Array, r: f64, g: f64, b: f64) -> bool {
        let s = self.window.inner_size();
        if s.width != self.width || s.height != self.height {
            self.configure();
        }
        let gfloats = (self.layout.global_size / 4) as usize;
        self.queue
            .write_buffer(&self.global_buf, 0, bytes(&globals.prefix(gfloats)));

        let texture = match self.surface.get_current_texture() {
            wgpu::CurrentSurfaceTexture::Success(t)
            | wgpu::CurrentSurfaceTexture::Suboptimal(t) => t,
            _ => {
                self.configure();
                return false;
            }
        };
        let view = texture.texture.create_view(&Default::default());
        let mut encoder = self.device.create_command_encoder(&Default::default());
        let pass = encoder
            .begin_render_pass(&wgpu::RenderPassDescriptor {
                label: None,
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &view,
                    depth_slice: None,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color { r, g, b, a: 1.0 }),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: self.depth_view.as_ref().map(|v| {
                    wgpu::RenderPassDepthStencilAttachment {
                        view: v,
                        depth_ops: Some(wgpu::Operations {
                            load: wgpu::LoadOp::Clear(1.0),
                            store: wgpu::StoreOp::Store,
                        }),
                        stencil_ops: None,
                    }
                }),
                ..Default::default()
            })
            .forget_lifetime();
        self.frame = Some(Frame {
            pass,
            encoder,
            texture,
            bound_verts: None,
            bound_pipe: None,
            pending: Vec::new(),
        });
        true
    }

    /// The sorted transparent phase, back to front, then submit and present.
    pub fn end(&mut self) {
        let Some(f) = self.frame.as_mut() else { return };
        let mut pending = std::mem::take(&mut f.pending);
        pending.sort_by(|a, b| b.depth.total_cmp(&a.depth));
        for d in pending {
            self.exec_draw(d, true);
        }
        let Some(Frame {
            pass,
            encoder,
            texture,
            ..
        }) = self.frame.take()
        else {
            return;
        };
        drop(pass);
        self.queue.submit([encoder.finish()]);
        self.queue.present(texture);
    }
}

fn buffer_entry(binding: u32, buffer: &wgpu::Buffer, size: u64) -> wgpu::BindGroupEntry<'_> {
    wgpu::BindGroupEntry {
        binding,
        resource: wgpu::BindingResource::Buffer(wgpu::BufferBinding {
            buffer,
            offset: 0,
            size: wgpu::BufferSize::new(size),
        }),
    }
}

fn shader(device: &wgpu::Device, wgsl: &str) -> wgpu::ShaderModule {
    device.create_shader_module(wgpu::ShaderModuleDescriptor {
        label: None,
        source: wgpu::ShaderSource::Wgsl(wgsl.into()),
    })
}

fn pipeline_layout(device: &wgpu::Device, bgl: &wgpu::BindGroupLayout) -> wgpu::PipelineLayout {
    device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
        label: None,
        bind_group_layouts: &[Some(bgl)],
        immediate_size: 0,
    })
}

fn upload_texture(
    device: &wgpu::Device,
    queue: &wgpu::Queue,
    rgba: &[u8],
    w: u32,
    h: u32,
    format: wgpu::TextureFormat,
) -> wgpu::TextureView {
    let size = wgpu::Extent3d {
        width: w,
        height: h,
        depth_or_array_layers: 1,
    };
    let t = device.create_texture(&wgpu::TextureDescriptor {
        label: None,
        size,
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format,
        usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
        view_formats: &[],
    });
    queue.write_texture(
        t.as_image_copy(),
        rgba,
        wgpu::TexelCopyBufferLayout {
            offset: 0,
            bytes_per_row: Some(4 * w),
            rows_per_image: Some(h),
        },
        size,
    );
    t.create_view(&Default::default())
}

enum PipelineKind<'a> {
    /// One vertex buffer: triangles or lines, opaque or blended.
    Plain {
        entry: &'static str,
        attrs: &'a [VertexAttr],
        stride: u64,
        lines: bool,
        blend: bool,
    },
    /// The mesh at vertex rate plus instance data at instance rate, opaque:
    /// a batch cannot be depth-sorted within itself.
    Instanced {
        mesh_attrs: &'a [VertexAttr],
        mesh_stride: u64,
        inst_attrs: &'a [VertexAttr],
        inst_stride: u64,
    },
}

fn make_pipeline(
    device: &wgpu::Device,
    format: wgpu::TextureFormat,
    module: &wgpu::ShaderModule,
    layout: &wgpu::PipelineLayout,
    kind: PipelineKind<'_>,
) -> wgpu::RenderPipeline {
    let vb = |attrs: &[VertexAttr], stride, step_mode| {
        (spec::vertex_attributes(attrs), stride, step_mode)
    };
    let (entry, buffers, lines, blend) = match kind {
        PipelineKind::Plain {
            entry,
            attrs,
            stride,
            lines,
            blend,
        } => (
            entry,
            vec![vb(attrs, stride, wgpu::VertexStepMode::Vertex)],
            lines,
            blend,
        ),
        PipelineKind::Instanced {
            mesh_attrs,
            mesh_stride,
            inst_attrs,
            inst_stride,
        } => (
            "vs_inst",
            vec![
                vb(mesh_attrs, mesh_stride, wgpu::VertexStepMode::Vertex),
                vb(inst_attrs, inst_stride, wgpu::VertexStepMode::Instance),
            ],
            false,
            false,
        ),
    };
    let layouts: Vec<Option<wgpu::VertexBufferLayout<'_>>> = buffers
        .iter()
        .map(|(attrs, stride, step_mode)| {
            Some(wgpu::VertexBufferLayout {
                array_stride: *stride,
                step_mode: *step_mode,
                attributes: attrs,
            })
        })
        .collect();
    let blend_state = wgpu::BlendState {
        color: wgpu::BlendComponent {
            src_factor: wgpu::BlendFactor::SrcAlpha,
            dst_factor: wgpu::BlendFactor::OneMinusSrcAlpha,
            operation: wgpu::BlendOperation::Add,
        },
        alpha: wgpu::BlendComponent {
            src_factor: wgpu::BlendFactor::One,
            dst_factor: wgpu::BlendFactor::OneMinusSrcAlpha,
            operation: wgpu::BlendOperation::Add,
        },
    };
    device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
        label: None,
        layout: Some(layout),
        vertex: wgpu::VertexState {
            module,
            entry_point: Some(entry),
            compilation_options: Default::default(),
            buffers: &layouts,
        },
        primitive: wgpu::PrimitiveState {
            topology: if lines {
                wgpu::PrimitiveTopology::LineList
            } else {
                wgpu::PrimitiveTopology::TriangleList
            },
            front_face: wgpu::FrontFace::Ccw,
            cull_mode: if lines { None } else { Some(wgpu::Face::Back) },
            ..Default::default()
        },
        depth_stencil: Some(wgpu::DepthStencilState {
            format: DEPTH_FORMAT,
            depth_write_enabled: Some(!blend),
            depth_compare: Some(wgpu::CompareFunction::Less),
            stencil: Default::default(),
            bias: Default::default(),
        }),
        multisample: Default::default(),
        fragment: Some(wgpu::FragmentState {
            module,
            entry_point: Some("fs"),
            compilation_options: Default::default(),
            targets: &[Some(wgpu::ColorTargetState {
                format,
                blend: blend.then_some(blend_state),
                write_mask: wgpu::ColorWrites::ALL,
            })],
        }),
        multiview_mask: None,
        cache: None,
    })
}
