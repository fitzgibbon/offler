//! The native context: one winit window, its wgpu renderer, gilrs pads and
//! the event report, behind the handle `offler_init` returns.
//!
//! The C returned NULL when SDL or wgpu could not start. A Rust handle is
//! never null, so a failed start returns a context with nothing in it: its
//! first poll reports a close, it never begins a frame, and everything else
//! no-ops, so the program ends at once after the reason went to stderr.

use std::cell::RefCell;
use std::time::{Duration, Instant};

use winit::event_loop::EventLoop;
use winit::platform::pump_events::{EventLoopExtPumpEvents, PumpStatus};
use winit::window::CursorGrabMode;

use crate::array::{F32Array, U32Array};
use crate::gamepad::{self, PadChange, Pads};
use crate::input::{Code, Collector, Input, Report};
use crate::render::{Gpu, Layout};
use crate::spec;

/// The renderer drops before the window's other holders, and the event
/// loop last of all.
struct Native {
    gpu: Gpu,
    input: Collector,
    pads: Pads,
    report: Report,
    cursor_visible: bool,
    event_loop: EventLoop<()>,
}

pub struct Ctx {
    start: Instant,
    native: RefCell<Option<Native>>,
}

impl Ctx {
    fn with<R>(&self, dead: R, f: impl FnOnce(&mut Native) -> R) -> R {
        match self.native.borrow_mut().as_mut() {
            Some(n) => f(n),
            None => dead,
        }
    }

    fn gpu<R>(&self, dead: R, f: impl FnOnce(&mut Gpu) -> R) -> R {
        self.with(dead, |n| f(&mut n.gpu))
    }
}

impl Native {
    /// Pump winit and gilrs into the queue, without blocking.
    fn pump(&mut self) {
        let status = self
            .event_loop
            .pump_app_events(Some(Duration::ZERO), &mut self.input);
        if let PumpStatus::Exit(_) = status {
            self.input.queue.push_back(Input::Close);
        }
        self.input.surface = (self.gpu.width, self.gpu.height);
        for c in self.pads.poll() {
            self.input.queue.push_back(pad_input(c));
        }
    }
}

fn pad_input(c: PadChange) -> Input {
    match c {
        PadChange::Added { id, name } => Input::PadAdded { id, name },
        PadChange::Removed { id } => Input::PadRemoved { id },
    }
}

fn start(
    title: &str,
    line_wgsl: &str,
    line_bind_spec: &str,
    layout: Layout,
) -> Result<Native, String> {
    let mut event_loop = EventLoop::new().map_err(|e| format!("event loop: {e}"))?;
    let mut input = Collector::new(title);
    for _ in 0..200 {
        if input.window.is_some() || input.create_error.is_some() {
            break;
        }
        if let PumpStatus::Exit(_) =
            event_loop.pump_app_events(Some(Duration::from_millis(5)), &mut input)
        {
            break;
        }
    }
    if let Some(e) = input.create_error.take() {
        return Err(format!("create window: {e}"));
    }
    let window = input.window.clone().ok_or("no window")?;
    let gpu = Gpu::new(window, layout, line_wgsl, line_bind_spec)?;
    input.surface = (gpu.width, gpu.height);
    let mut pads = Pads::new();
    for c in pads.initial() {
        input.queue.push_back(pad_input(c));
    }
    Ok(Native {
        event_loop,
        input,
        pads,
        report: Report::default(),
        gpu,
        cursor_visible: true,
    })
}

// ------------------------------------------------------------------- init

#[allow(clippy::too_many_arguments)]
pub fn offler_init(
    title: &str,
    line_wgsl: &str,
    line_bind_spec: &str,
    line_vert_spec: &str,
    line_stride: i64,
    mesh_vert_spec: &str,
    mesh_stride: i64,
    inst_vert_spec: &str,
    inst_stride: i64,
    global_size: i64,
    obj_size: i64,
    obj_stride: i64,
    max_objects: i64,
) -> Ctx {
    let start_time = Instant::now();
    let size = |n: i64| n as i32 as u64;
    let layout = Layout {
        global_size: size(global_size),
        obj_size: size(obj_size),
        obj_stride: size(obj_stride),
        max_objects: i64::from(max_objects as i32),
        mesh_stride: size(mesh_stride),
        line_stride: size(line_stride),
        inst_stride: size(inst_stride),
        mesh_attrs: spec::parse_attrs(mesh_vert_spec),
        line_attrs: spec::parse_attrs(line_vert_spec),
        inst_attrs: spec::parse_attrs(inst_vert_spec),
    };
    let native = match start(title, line_wgsl, line_bind_spec, layout) {
        Ok(n) => Some(n),
        Err(e) => {
            eprintln!("offler: {e}");
            None
        }
    };
    Ctx {
        start: start_time,
        native: RefCell::new(native),
    }
}

// ------------------------------------------------------------ registration

pub fn offler_register_material(
    c: &Ctx,
    wgsl: &str,
    bind_spec: &str,
    has_line: i64,
    has_inst: i64,
) -> i64 {
    c.gpu(0, |g| {
        g.register_material(wgsl, bind_spec, has_line as i32 != 0, has_inst as i32 != 0)
    })
}

// ---------------------------------------------------------------- textures

pub fn offler_texture_file(c: &Ctx, path: &str) -> i64 {
    let img = image::ImageReader::open(path)
        .and_then(|r| r.with_guessed_format())
        .map_err(image::ImageError::IoError)
        .and_then(|r| r.decode());
    c.gpu(0, |g| match img {
        Ok(i) => texture(g, i),
        Err(e) => {
            eprintln!("offler: could not decode {path}: {e}");
            g.white_entry()
        }
    })
}

pub fn offler_texture_b64(c: &Ctx, b64: &str) -> i64 {
    let img = decode_b64(b64)
        .map_err(|e| e.to_string())
        .and_then(|bytes| image::load_from_memory(&bytes).map_err(|e| e.to_string()));
    c.gpu(0, |g| match img {
        Ok(i) => texture(g, i),
        Err(e) => {
            eprintln!("offler: could not decode embedded image: {e}");
            g.white_entry()
        }
    })
}

fn texture(g: &mut Gpu, img: image::DynamicImage) -> i64 {
    let rgba = img.to_rgba8();
    let (w, h) = rgba.dimensions();
    if w == 0 || h == 0 {
        return g.white_entry();
    }
    g.add_texture(&rgba, w, h)
}

/// Base64 as the C read it: line breaks skipped, padding optional.
pub fn decode_b64(s: &str) -> Result<Vec<u8>, base64::DecodeError> {
    use base64::Engine;
    use base64::engine::{DecodePaddingMode, GeneralPurpose, GeneralPurposeConfig};
    const LENIENT: GeneralPurpose = GeneralPurpose::new(
        &base64::alphabet::STANDARD,
        GeneralPurposeConfig::new()
            .with_decode_padding_mode(DecodePaddingMode::Indifferent)
            .with_decode_allow_trailing_bits(true),
    );
    let clean: String = s
        .chars()
        .filter(|&c| c != '\n' && c != '\r' && c != '=')
        .collect();
    LENIENT.decode(clean)
}

// ---------------------------------------------------------- window & input

pub fn offler_set_title(c: &Ctx, s: &str) {
    c.gpu((), |g| g.window().set_title(s));
}

/// Seconds since start, at the millisecond resolution of `SDL_GetTicks`.
pub fn offler_time(c: &Ctx) -> f64 {
    c.start.elapsed().as_millis() as f64 * 0.001
}

pub fn offler_aspect(c: &Ctx) -> f64 {
    c.gpu(1.0, |g| f64::from(g.width) / f64::from(g.height))
}

pub fn offler_surface_w(c: &Ctx) -> f64 {
    c.gpu(1.0, |g| f64::from(g.width))
}

pub fn offler_surface_h(c: &Ctx) -> f64 {
    c.gpu(1.0, |g| f64::from(g.height))
}

/// One event per call, 0 when the queue is empty; see `input::Code`.
pub fn offler_poll(c: &Ctx) -> i64 {
    c.with(Code::Close as i64, |n| {
        let mut code = n.report.next(&mut n.input.queue);
        if code == Code::Empty {
            n.pump();
            code = n.report.next(&mut n.input.queue);
        }
        code as i64
    })
}

pub fn offler_event_key(c: &Ctx) -> String {
    c.with(String::new(), |n| n.report.key.clone())
}

pub fn offler_event_x(c: &Ctx) -> f64 {
    c.with(0.0, |n| n.report.x)
}

pub fn offler_event_y(c: &Ctx) -> f64 {
    c.with(0.0, |n| n.report.y)
}

pub fn offler_event_dx(c: &Ctx) -> f64 {
    c.with(0.0, |n| n.report.dx)
}

pub fn offler_event_dy(c: &Ctx) -> f64 {
    c.with(0.0, |n| n.report.dy)
}

pub fn offler_event_wheel(c: &Ctx) -> f64 {
    c.with(0.0, |n| n.report.wheel)
}

pub fn offler_event_button(c: &Ctx) -> i64 {
    c.with(0, |n| n.report.button)
}

pub fn offler_event_id(c: &Ctx) -> i64 {
    c.with(0, |n| n.report.id)
}

pub fn offler_gamepads(c: &Ctx) -> String {
    c.with(String::new(), |n| gamepad::format_pads(&n.pads.sample()))
}

/// Locked where the platform can, confined where it cannot (X11). The
/// change event is queued at once, as SDL granted synchronously.
pub fn offler_set_pointer_lock(c: &Ctx, on: i64) {
    let on = on as i32 != 0;
    c.with((), |n| {
        let w = n.gpu.window();
        let ok = if on {
            w.set_cursor_grab(CursorGrabMode::Locked)
                .or_else(|_| w.set_cursor_grab(CursorGrabMode::Confined))
                .is_ok()
        } else {
            w.set_cursor_grab(CursorGrabMode::None).is_ok()
        };
        if ok {
            // Relative mode hides the cursor while it lasts, as SDL's did.
            w.set_cursor_visible(!on && n.cursor_visible);
            n.input.relative = on;
            n.report.lock_changed(on);
        }
    });
}

pub fn offler_set_cursor_visible(c: &Ctx, on: i64) {
    let on = on as i32 != 0;
    c.with((), |n| {
        n.cursor_visible = on;
        if !n.input.relative {
            n.gpu.window().set_cursor_visible(on);
        }
    });
}

pub fn offler_resize(c: &Ctx) {
    c.gpu((), Gpu::configure);
}

/// Close the window and release the device. The handle stays valid and
/// dead, as the C's freed pointer was never used again.
pub fn offler_quit(c: &Ctx) {
    c.native.borrow_mut().take();
}

// ------------------------------------------------------------ paged buffers

pub fn offler_upload_obj_page(c: &Ctx, page: i64, data: &F32Array, float_count: i64) {
    c.gpu((), |g| g.upload_obj_page(page, data, float_count));
}

pub fn offler_upload_mat_slot(c: &Ctx, mat_data: &F32Array, local: i64, global: i64) {
    c.gpu((), |g| g.upload_mat_slot(mat_data, local, global));
}

// ------------------------------------------------------------------ meshes

pub fn offler_mesh_gen(c: &Ctx, i: i64) -> i64 {
    c.gpu(0, |g| g.mesh_gen(i))
}

pub fn offler_create_mesh(c: &Ctx, verts: &F32Array, vertex_count: i64, topo: i64) -> i64 {
    c.gpu(0, |g| g.create_mesh(verts, vertex_count, topo))
}

pub fn offler_create_mesh_indexed(
    c: &Ctx,
    verts: &F32Array,
    vertex_count: i64,
    idx: &U32Array,
    idx_count: i64,
) -> i64 {
    c.gpu(0, |g| {
        g.create_mesh_indexed(verts, vertex_count, idx, idx_count)
    })
}

pub fn offler_free_mesh(c: &Ctx, mi: i64, generation: i64) {
    c.gpu((), |g| g.free_mesh(mi, generation));
}

pub fn offler_set_lines(c: &Ctx, verts: &F32Array, float_count: i64, vertex_count: i64) {
    c.gpu((), |g| g.set_lines(verts, float_count, vertex_count));
}

// ----------------------------------------------------------------- drawing

pub fn offler_add_asset(c: &Ctx, mat: i64, slot: i64, t0: i64, t1: i64, t2: i64, t3: i64) -> i64 {
    c.gpu(0, |g| g.add_asset(mat, slot, [t0, t1, t2, t3]))
}

pub fn offler_update_asset(c: &Ctx, ai: i64, t0: i64, t1: i64, t2: i64, t3: i64) {
    c.gpu((), |g| g.update_asset(ai, [t0, t1, t2, t3]));
}

pub fn offler_draw(
    c: &Ctx,
    asset: i64,
    mesh: i64,
    generation: i64,
    slot: i64,
    blend: i64,
    depth: f64,
) {
    c.gpu((), |g| {
        g.draw_slices(asset, mesh, generation, slot, 1, blend as i32 != 0, depth)
    });
}

#[allow(clippy::too_many_arguments)]
pub fn offler_draw_slices(
    c: &Ctx,
    asset: i64,
    mesh: i64,
    generation: i64,
    first: i64,
    count: i64,
    blend: i64,
    depth: f64,
) {
    c.gpu((), |g| {
        g.draw_slices(
            asset,
            mesh,
            generation,
            first,
            count,
            blend as i32 != 0,
            depth,
        )
    });
}

pub fn offler_create_instances(c: &Ctx) -> i64 {
    c.gpu(0, Gpu::create_instances)
}

pub fn offler_write_instances(c: &Ctx, ih: i64, data: &F32Array, float_count: i64, count: i64) {
    c.gpu((), |g| g.write_instances(ih, data, float_count, count));
}

pub fn offler_draw_instanced(c: &Ctx, asset: i64, mesh: i64, generation: i64, slot: i64, ih: i64) {
    c.gpu((), |g| g.draw_instanced(asset, mesh, generation, slot, ih));
}

pub fn offler_draw_lines(c: &Ctx, slot: i64) {
    c.gpu((), |g| g.draw_lines(slot));
}

/// 0 if the frame cannot be started, so Idris can skip it.
pub fn offler_begin(c: &Ctx, globals: &F32Array, r: f64, g: f64, b: f64) -> i64 {
    c.gpu(0, |gpu| i64::from(gpu.begin(globals, r, g, b)))
}

pub fn offler_end(c: &Ctx) {
    c.gpu((), Gpu::end);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn base64_is_lenient_like_the_c() {
        assert_eq!(decode_b64("aGVsbG8=").unwrap(), b"hello");
        assert_eq!(decode_b64("aGVs\r\nbG8").unwrap(), b"hello");
        assert_eq!(decode_b64("").unwrap(), b"");
    }

    #[test]
    fn embedded_png_decodes() {
        // A 1x1 opaque red PNG.
        let png = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8DwHwAFBQIAX8jx0gAAAABJRU5ErkJggg==";
        let img = image::load_from_memory(&decode_b64(png).unwrap())
            .unwrap()
            .to_rgba8();
        assert_eq!(img.dimensions(), (1, 1));
    }

    #[test]
    fn a_dead_context_closes_and_no_ops() {
        let c = Ctx {
            start: Instant::now(),
            native: RefCell::new(None),
        };
        assert_eq!(offler_poll(&c), 1);
        assert_eq!(
            offler_begin(&c, &crate::array::offler_f32_new(72), 0.0, 0.0, 0.0),
            0
        );
        assert_eq!(offler_aspect(&c), 1.0);
        assert_eq!(offler_gamepads(&c), "");
        offler_end(&c);
        offler_quit(&c);
        assert!(offler_time(&c) >= 0.0);
    }
}
