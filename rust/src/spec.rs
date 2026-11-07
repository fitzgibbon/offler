//! The layout spec strings `Offler.Gfx.Layout` derives, parsed exactly as
//! the C shim's `strtol` walk did: records separated by `;`, fields by `,`,
//! a malformed field reading as zero.

/// At most this many bind entries and vertex attributes per spec, as in C.
pub const MAX_BINDINGS: usize = 16;
pub const MAX_ATTRS: usize = 8;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BindEntry {
    /// `0,slot,visibility,dynamic,minBindingSize`
    Uniform {
        slot: u32,
        visibility: u32,
        dynamic: bool,
        min_size: u64,
    },
    /// `1,slot`: a fragment-stage 2D float texture.
    Texture { slot: u32 },
    /// `2,slot` (or any other kind): a fragment-stage filtering sampler.
    Sampler { slot: u32 },
}

/// A parsed bind spec, with what a material needs from it: the size of
/// its uniform at slot 2 (0 when absent) and how many textures it binds.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BindSpec {
    pub entries: Vec<BindEntry>,
    pub mat_size: u64,
    pub tex_count: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct VertexAttr {
    pub location: u32,
    pub offset: u64,
    /// Float components: 1, 2, 4, or 3 for anything else.
    pub components: u32,
}

/// A cursor over a spec string with C's `strtol` + skip-one-comma step.
struct Cursor<'a>(&'a [u8]);

impl Cursor<'_> {
    fn done(&self) -> bool {
        self.0.is_empty()
    }

    /// `strtol(s, &s, 10)` then skip one `,`. Leading whitespace and a sign
    /// are accepted; no digits reads as zero without consuming anything.
    fn int(&mut self) -> i64 {
        let s = self.0;
        let mut i = 0;
        while i < s.len() && s[i].is_ascii_whitespace() {
            i += 1;
        }
        let neg = i < s.len() && s[i] == b'-';
        if i < s.len() && (s[i] == b'-' || s[i] == b'+') {
            i += 1;
        }
        let start = i;
        let mut v: i64 = 0;
        while i < s.len() && s[i].is_ascii_digit() {
            v = v.saturating_mul(10).saturating_add(i64::from(s[i] - b'0'));
            i += 1;
        }
        if i > start {
            self.0 = &s[i..];
        }
        if self.0.first() == Some(&b',') {
            self.0 = &self.0[1..];
        }
        if neg { -v } else { v }
    }

    fn end_record(&mut self) {
        if self.0.first() == Some(&b';') {
            self.0 = &self.0[1..];
        }
    }
}

/// The C stored `strtol`'s long into an `int`, then cast as each field needed.
fn c_int(v: i64) -> i32 {
    v as i32
}

pub fn parse_bind_spec(s: &str) -> BindSpec {
    let mut c = Cursor(s.as_bytes());
    let mut entries = Vec::new();
    let mut mat_size = 0;
    let mut tex_count = 0;
    while !c.done() && entries.len() < MAX_BINDINGS {
        let kind = c_int(c.int());
        let slot = c_int(c.int()) as u32;
        let entry = match kind {
            0 => {
                let visibility = c_int(c.int()) as u32;
                let dynamic = c_int(c.int()) != 0;
                let min_size = i64::from(c_int(c.int())) as u64;
                if slot == 2 {
                    mat_size = min_size;
                }
                BindEntry::Uniform {
                    slot,
                    visibility,
                    dynamic,
                    min_size,
                }
            }
            1 => {
                tex_count += 1;
                BindEntry::Texture { slot }
            }
            _ => BindEntry::Sampler { slot },
        };
        c.end_record();
        entries.push(entry);
    }
    BindSpec {
        entries,
        mat_size,
        tex_count,
    }
}

pub fn parse_attrs(s: &str) -> Vec<VertexAttr> {
    let mut c = Cursor(s.as_bytes());
    let mut out = Vec::new();
    while !c.done() && out.len() < MAX_ATTRS {
        let location = c_int(c.int()) as u32;
        let offset = i64::from(c_int(c.int())) as u64;
        let components = match c_int(c.int()) {
            n @ (1 | 2 | 4) => n as u32,
            _ => 3,
        };
        c.end_record();
        out.push(VertexAttr {
            location,
            offset,
            components,
        });
    }
    out
}

pub fn bind_entries(spec: &BindSpec) -> Vec<wgpu::BindGroupLayoutEntry> {
    spec.entries
        .iter()
        .map(|e| match *e {
            BindEntry::Uniform {
                slot,
                visibility,
                dynamic,
                min_size,
            } => wgpu::BindGroupLayoutEntry {
                binding: slot,
                visibility: wgpu::ShaderStages::from_bits_truncate(visibility),
                ty: wgpu::BindingType::Buffer {
                    ty: wgpu::BufferBindingType::Uniform,
                    has_dynamic_offset: dynamic,
                    min_binding_size: wgpu::BufferSize::new(min_size),
                },
                count: None,
            },
            BindEntry::Texture { slot } => wgpu::BindGroupLayoutEntry {
                binding: slot,
                visibility: wgpu::ShaderStages::FRAGMENT,
                ty: wgpu::BindingType::Texture {
                    sample_type: wgpu::TextureSampleType::Float { filterable: true },
                    view_dimension: wgpu::TextureViewDimension::D2,
                    multisampled: false,
                },
                count: None,
            },
            BindEntry::Sampler { slot } => wgpu::BindGroupLayoutEntry {
                binding: slot,
                visibility: wgpu::ShaderStages::FRAGMENT,
                ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                count: None,
            },
        })
        .collect()
}

pub fn vertex_attributes(attrs: &[VertexAttr]) -> Vec<wgpu::VertexAttribute> {
    attrs
        .iter()
        .map(|a| wgpu::VertexAttribute {
            format: match a.components {
                1 => wgpu::VertexFormat::Float32,
                2 => wgpu::VertexFormat::Float32x2,
                4 => wgpu::VertexFormat::Float32x4,
                _ => wgpu::VertexFormat::Float32x3,
            },
            offset: a.offset,
            shader_location: a.location,
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_a_material_bind_spec() {
        let s = parse_bind_spec("0,0,3,0,288;0,1,3,1,80;0,2,3,1,48;1,3;2,4;1,5;2,6");
        assert_eq!(s.mat_size, 48);
        assert_eq!(s.tex_count, 2);
        assert_eq!(
            s.entries[..3],
            [
                BindEntry::Uniform {
                    slot: 0,
                    visibility: 3,
                    dynamic: false,
                    min_size: 288
                },
                BindEntry::Uniform {
                    slot: 1,
                    visibility: 3,
                    dynamic: true,
                    min_size: 80
                },
                BindEntry::Uniform {
                    slot: 2,
                    visibility: 3,
                    dynamic: true,
                    min_size: 48
                },
            ]
        );
        assert_eq!(
            s.entries[3..],
            [
                BindEntry::Texture { slot: 3 },
                BindEntry::Sampler { slot: 4 },
                BindEntry::Texture { slot: 5 },
                BindEntry::Sampler { slot: 6 },
            ]
        );
    }

    #[test]
    fn line_spec_has_no_material_block() {
        let s = parse_bind_spec("0,0,3,0,288;0,1,3,1,80");
        assert_eq!(s.entries.len(), 2);
        assert_eq!(s.mat_size, 0);
        assert_eq!(s.tex_count, 0);
    }

    #[test]
    fn unknown_kinds_are_samplers_and_empty_is_empty() {
        assert_eq!(
            parse_bind_spec("7,9").entries,
            vec![BindEntry::Sampler { slot: 9 }]
        );
        assert!(parse_bind_spec("").entries.is_empty());
    }

    #[test]
    fn bind_entries_are_capped() {
        let spec = ["1,0"; 20].join(";");
        let s = parse_bind_spec(&spec);
        assert_eq!(s.entries.len(), MAX_BINDINGS);
        assert_eq!(s.tex_count, MAX_BINDINGS);
    }

    #[test]
    fn parses_vertex_attributes() {
        let a = parse_attrs("0,0,3;1,12,3;2,24,2");
        assert_eq!(
            a,
            vec![
                VertexAttr {
                    location: 0,
                    offset: 0,
                    components: 3
                },
                VertexAttr {
                    location: 1,
                    offset: 12,
                    components: 3
                },
                VertexAttr {
                    location: 2,
                    offset: 24,
                    components: 2
                },
            ]
        );
        let i = parse_attrs("3,0,4;4,16,4;5,32,4;6,48,4;7,64,4");
        assert_eq!(i.len(), 5);
        assert_eq!(i[4].location, 7);
        assert_eq!(i[4].offset, 64);
    }

    #[test]
    fn odd_component_counts_are_vec3_and_attrs_are_capped() {
        assert_eq!(parse_attrs("0,0,7")[0].components, 3);
        assert_eq!(parse_attrs("0,0,1")[0].components, 1);
        let spec = ["0,0,4"; 12].join(";");
        assert_eq!(parse_attrs(&spec).len(), MAX_ATTRS);
    }

    #[test]
    fn malformed_fields_read_as_zero_like_strtol() {
        let a = parse_attrs("x");
        // strtol consumes nothing, so the C loop would never advance past
        // `x`; it stops at MAX_ATTRS zeroed attributes, and so does this.
        assert_eq!(a.len(), MAX_ATTRS);
        assert!(
            a.iter()
                .all(|v| v.location == 0 && v.offset == 0 && v.components == 3)
        );
        assert_eq!(parse_attrs(" 1,+8,-2")[0].offset, 8);
    }

    #[test]
    fn converts_to_wgpu_formats() {
        let v = vertex_attributes(&parse_attrs("0,0,1;1,4,2;2,12,3;3,24,4"));
        let f: Vec<_> = v.iter().map(|a| a.format).collect();
        assert_eq!(
            f,
            vec![
                wgpu::VertexFormat::Float32,
                wgpu::VertexFormat::Float32x2,
                wgpu::VertexFormat::Float32x3,
                wgpu::VertexFormat::Float32x4
            ]
        );
        let b = bind_entries(&parse_bind_spec("0,1,3,1,80"));
        assert_eq!(
            b[0].visibility,
            wgpu::ShaderStages::VERTEX | wgpu::ShaderStages::FRAGMENT
        );
    }
}
