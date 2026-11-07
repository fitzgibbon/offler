//! A headless smoke run of the whole adapter, driven the way the Idris
//! side drives it: init with the layout's specs, register a material,
//! upload meshes, textures and instances, then a few frames of every draw
//! kind, input polling and teardown. `SMOKE_FRAMES` sets the frame count.
//! `SMOKE_SHADERS` names a directory of shaders dumped from the Idris
//! side (`line.wgsl`, and `<material>.wgsl` with its `<material>.spec`): the
//! gizmo pipeline and the drawn material then use the real ones, and every
//! other material there is registered too, which validates its pipelines.

use idris2_rs_foreign::*;

const LINE_PROLOGUE: &str = "
struct Globals { proj : mat4x4<f32>, view : mat4x4<f32>, rest : array<vec4<f32>, 10> };
struct Obj { model : mat4x4<f32>, lane : vec4<f32> };
struct LineIn { @location(0) pos : vec4<f32>, @location(1) color : vec4<f32> };
@group(0) @binding(0) var<uniform> g : Globals;
@group(0) @binding(1) var<uniform> o : Obj;
";

const MATERIAL: &str = "
struct Globals { proj : mat4x4<f32>, view : mat4x4<f32>, rest : array<vec4<f32>, 10> };
struct Obj { model : mat4x4<f32>, lane : vec4<f32> };
struct Mat { color : vec4<f32> };
@group(0) @binding(0) var<uniform> g : Globals;
@group(0) @binding(1) var<uniform> o : Obj;
@group(0) @binding(2) var<uniform> m : Mat;
@group(0) @binding(3) var t0 : texture_2d<f32>;
@group(0) @binding(4) var s0 : sampler;
struct VIn { @location(0) pos : vec3<f32>, @location(1) normal : vec3<f32>, @location(2) uv : vec2<f32> };
struct LIn { @location(0) pos : vec4<f32>, @location(1) color : vec4<f32> };
struct IIn { @location(3) im0 : vec4<f32>, @location(4) im1 : vec4<f32>, @location(5) im2 : vec4<f32>,
             @location(6) im3 : vec4<f32>, @location(7) icolor : vec4<f32> };
struct Out { @builtin(position) pos : vec4<f32>, @location(0) uv : vec2<f32>, @location(1) color : vec4<f32> };
@vertex fn vs(v : VIn) -> Out {
  return Out(g.proj * g.view * o.model * vec4<f32>(v.pos, 1.0), v.uv, o.lane);
}
@vertex fn vs_line(v : LIn) -> Out {
  return Out(g.proj * g.view * o.model * vec4<f32>(v.pos.xyz, 1.0), vec2<f32>(0.0), v.color);
}
@vertex fn vs_inst(v : VIn, i : IIn) -> Out {
  let im = mat4x4<f32>(i.im0, i.im1, i.im2, i.im3);
  return Out(g.proj * g.view * im * vec4<f32>(v.pos, 1.0), v.uv, i.icolor);
}
@fragment fn fs(in : Out) -> @location(0) vec4<f32> {
  return textureSample(t0, s0, in.uv) * m.color * in.color;
}
";

const BIND_SPEC: &str = "0,0,3,0,288;0,1,3,1,80;0,2,3,1,16;1,3;2,4";
const RED_PNG: &str = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8DwHwAFBQIAX8jx0gAAAABJRU5ErkJggg==";
const MAX_OBJECTS: i64 = 10240;

fn identity(a: &F32Array, at: i64) {
    for k in 0..16 {
        offler_f32_poke(a, at + k, if k % 5 == 0 { 1.0 } else { 0.0 });
    }
}

fn fill(a: &F32Array, xs: &[f64]) {
    for (i, &x) in xs.iter().enumerate() {
        offler_f32_poke(a, i as i64, x);
    }
}

fn main() {
    let frames: i64 = std::env::var("SMOKE_FRAMES")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(3);
    let dir = std::env::var("SMOKE_SHADERS")
        .ok()
        .filter(|d| !d.is_empty());
    let read = |f: &str| {
        dir.as_ref()
            .map(|d| std::fs::read_to_string(format!("{d}/{f}")).expect(f))
    };
    let line_wgsl = read("line.wgsl").unwrap_or_else(|| {
        format!(
            "{LINE_PROLOGUE}{}",
            include_str!("../../shaders/lines.wgsl")
        )
    });
    let c = offler_init(
        "smoke",
        &line_wgsl,
        "0,0,3,0,288;0,1,3,1,80",
        "0,0,4;1,16,4",
        32,
        "0,0,3;1,12,3;2,24,2",
        32,
        "3,0,4;4,16,4;5,32,4;6,48,4;7,64,4",
        80,
        288,
        80,
        256,
        MAX_OBJECTS,
    );
    println!("surface {}x{}", offler_surface_w(&c), offler_surface_h(&c));

    let (wgsl, spec) = match (read("standard.wgsl"), read("standard.spec")) {
        (Some(w), Some(s)) => (w, s),
        _ => (MATERIAL.to_owned(), BIND_SPEC.to_owned()),
    };
    let mat = offler_register_material(&c, &wgsl, &spec, 1, 1);
    if let Some(d) = &dir {
        for e in std::fs::read_dir(d).expect("shader dir") {
            let p = e.expect("entry").path();
            let stem = p
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or_default()
                .to_owned();
            if p.extension().is_some_and(|x| x == "spec") && stem != "line" && stem != "standard" {
                let w = read(&format!("{stem}.wgsl")).expect("wgsl beside spec");
                let id = offler_register_material(
                    &c,
                    &w,
                    &read(&format!("{stem}.spec")).expect("spec"),
                    0,
                    0,
                );
                println!("registered {stem} as {id}");
            }
        }
    }
    let red = offler_texture_b64(&c, RED_PNG);
    let missing = offler_texture_file(&c, "/nonexistent.png");

    // One material slot: white.
    let mats = offler_f32_new(64);
    fill(&mats, &[1.0, 1.0, 1.0, 1.0]);
    offler_upload_mat_slot(&c, &mats, 0, 0);
    let opaque = offler_add_asset(&c, mat, 0, -1, 0, 0, 0);
    let textured = offler_add_asset(&c, mat, 0, red, 0, 0, 0);
    offler_update_asset(&c, textured, missing, 0, 0, 0);
    offler_update_asset(&c, textured, red, 0, 0, 0);

    #[rustfmt::skip]
    let tri = [
        -0.5, -0.5, 0.0,  0.0, 0.0, 1.0,  0.0, 0.0,
         0.5, -0.5, 0.0,  0.0, 0.0, 1.0,  1.0, 0.0,
         0.0,  0.5, 0.0,  0.0, 0.0, 1.0,  0.5, 1.0,
    ];
    let verts = offler_f32_new(24);
    fill(&verts, &tri);
    let mesh = offler_create_mesh(&c, &verts, 3, 0);
    let idx = offler_u32_new(3);
    for k in 0..3 {
        offler_u32_poke(&idx, k, k);
    }
    let indexed = offler_create_mesh_indexed(&c, &verts, 3, &idx, 3);
    let line_verts = offler_f32_new(16);
    fill(
        &line_verts,
        &[
            -1.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 1.0,
        ],
    );
    let line_mesh = offler_create_mesh(&c, &line_verts, 2, 1);
    offler_set_lines(&c, &line_verts, 16, 2);

    // A freed mesh's handle goes stale; its index is recycled.
    let gone = offler_create_mesh(&c, &verts, 3, 0);
    let gone_gen = offler_mesh_gen(&c, gone);
    offler_free_mesh(&c, gone, gone_gen);
    offler_free_mesh(&c, gone, gone_gen);
    let again = offler_create_mesh(&c, &verts, 3, 0);
    assert_eq!(again, gone);
    assert_eq!(offler_mesh_gen(&c, again), gone_gen + 1);

    let ih = offler_create_instances(&c);
    let inst = offler_f32_new(40);
    for k in 0..2 {
        identity(&inst, k * 20);
        offler_f32_poke(&inst, k * 20 + 12, k as f64 * 0.3 - 0.6);
        offler_f32_poke4(&inst, k * 20 + 16, 0.2, 0.9, 0.2, 1.0);
    }
    offler_write_instances(&c, ih, &inst, 40, 2);

    let globals = offler_f32_new(72);
    identity(&globals, 0);
    identity(&globals, 16);
    let objs = offler_f32_new(MAX_OBJECTS * 64);

    let au = offler_audio_init();
    let pcm = offler_f32_new(4800);
    for k in 0..4800 {
        offler_f32_poke(&pcm, k, (k as f64 * 0.05).sin() * 0.1);
    }
    let snd = offler_audio_load_pcm(&au, 48_000, &pcm, 4800);
    let voice = offler_audio_play(&au, snd, 0.5, 0.0, 1.0, 0);
    println!("sound {snd} voice {voice}");

    let mut drawn = 0;
    let mut events = 0;
    for f in 0..frames {
        loop {
            match offler_poll(&c) {
                0 => break,
                1 => {
                    println!("close requested");
                    break;
                }
                _ => events += 1,
            }
        }
        if f == 1 {
            offler_set_title(&c, "smoke  |  frame 1");
            offler_set_pointer_lock(&c, 1);
            offler_set_cursor_visible(&c, 1);
        }
        if f == 2 {
            offler_set_pointer_lock(&c, 0);
        }
        if offler_begin(&c, &globals, 0.1, 0.2, 0.3) == 0 {
            continue;
        }
        let mut slot = 0;
        let mut put = |x: f64, lane: [f64; 4]| {
            let at = slot * 64;
            identity(&objs, at);
            offler_f32_poke(&objs, at + 12, x);
            offler_f32_poke4(&objs, at + 16, lane[0], lane[1], lane[2], lane[3]);
            slot += 1;
            slot - 1
        };
        let s = put(-0.5, [1.0, 1.0, 1.0, 1.0]);
        offler_draw(&c, opaque, mesh, offler_mesh_gen(&c, mesh), s, 0, 0.0);
        let s = put(0.5, [1.0, 1.0, 1.0, 0.5]);
        offler_draw(
            &c,
            textured,
            indexed,
            offler_mesh_gen(&c, indexed),
            s,
            1,
            2.0,
        );
        let s0 = put(0.0, [0.0, 0.0, 1.0, 0.5]);
        put(0.1, [0.0, 0.0, 1.0, 0.5]);
        offler_draw_slices(&c, opaque, mesh, offler_mesh_gen(&c, mesh), s0, 2, 1, 1.0);
        let s = put(0.0, [1.0, 1.0, 1.0, 1.0]);
        offler_draw(
            &c,
            opaque,
            line_mesh,
            offler_mesh_gen(&c, line_mesh),
            s,
            0,
            0.0,
        );
        let s = put(0.0, [1.0, 1.0, 1.0, 1.0]);
        offler_draw_instanced(&c, opaque, mesh, offler_mesh_gen(&c, mesh), s, ih);
        // A stale generation draws nothing.
        offler_draw(&c, opaque, again, gone_gen, put(0.0, [1.0; 4]), 0, 0.0);
        let s = put(0.0, [1.0, 1.0, 1.0, 1.0]);
        offler_draw_lines(&c, s);
        offler_upload_obj_page(&c, 0, &objs, slot * 64);
        offler_end(&c);
        drawn += 1;
    }
    println!(
        "frames {drawn}/{frames} events {events} aspect {:.3} pads [{}] t {:.3}",
        offler_aspect(&c),
        offler_gamepads(&c),
        offler_time(&c)
    );
    offler_quit(&c);
    assert!(drawn > 0, "no frame was drawn");
    println!("smoke ok");
}
