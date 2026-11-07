//! Gamepads through gilrs, sampled into the wire form `parseGamepads`
//! reads: `id,lx,ly,rx,ry,lt,rt,buttons,name`, records joined by `;`.

use gilrs::{Axis, Button, GamepadId, Gilrs};

/// The C's 4096-byte buffer, filled while 160 bytes of headroom remained.
const SPEC_BYTES: usize = 4096;
const RECORD_HEADROOM: usize = 160;
/// Names were copied through a 96-byte buffer.
const NAME_BYTES: usize = 95;

/// Bit order matches `Offler.Gfx.Platform.buttonBit`.
const BUTTON_BITS: [Button; 14] = [
    Button::South,
    Button::East,
    Button::West,
    Button::North,
    Button::LeftTrigger,
    Button::RightTrigger,
    Button::Select,
    Button::Start,
    Button::LeftThumb,
    Button::RightThumb,
    Button::DPadUp,
    Button::DPadDown,
    Button::DPadLeft,
    Button::DPadRight,
];

/// One pad's state in SDL's conventions: sticks in -1..1 with +y down,
/// triggers in 0..1.
#[derive(Clone, Debug, PartialEq)]
pub struct PadSample {
    pub id: i64,
    pub axes: [f64; 6],
    pub buttons: u32,
    pub name: String,
}

pub fn format_pads(pads: &[PadSample]) -> String {
    let mut out = String::new();
    for (i, p) in pads.iter().enumerate() {
        if out.len() + RECORD_HEADROOM >= SPEC_BYTES {
            break;
        }
        let name = crate::keys::truncate(&p.name, NAME_BYTES).replace(';', " ");
        let a = p.axes.map(|v| v.max(-1.0));
        out.push_str(&format!(
            "{}{},{:.4},{:.4},{:.4},{:.4},{:.4},{:.4},{},{}",
            if i > 0 { ";" } else { "" },
            p.id,
            a[0],
            a[1],
            a[2],
            a[3],
            a[4],
            a[5],
            p.buttons,
            name
        ));
    }
    out
}

/// What a gilrs event meant to offler's queue.
pub enum PadChange {
    Added { id: i64, name: String },
    Removed { id: i64 },
}

/// The opened pads, in the order the C kept them: appended on connection,
/// swap-removed on disconnection.
pub struct Pads {
    gilrs: Option<Gilrs>,
    open: Vec<GamepadId>,
}

/// SDL's instance ids were C `int`s; gilrs's are indices.
fn pad_id(id: GamepadId) -> i64 {
    usize::from(id) as i32 as i64
}

impl Pads {
    pub fn new() -> Self {
        let gilrs = match Gilrs::new() {
            Ok(g) => Some(g),
            Err(gilrs::Error::NotImplemented(g)) => Some(g),
            Err(e) => {
                eprintln!("offler: gamepads unavailable: {e}");
                None
            }
        };
        Self {
            gilrs,
            open: Vec::new(),
        }
    }

    /// Pads present before the first poll, reported as SDL reported them:
    /// as additions.
    pub fn initial(&mut self) -> Vec<PadChange> {
        let Some(g) = &self.gilrs else {
            return Vec::new();
        };
        let ids: Vec<GamepadId> = g.gamepads().map(|(id, _)| id).collect();
        ids.into_iter().filter_map(|id| self.added(id)).collect()
    }

    /// Drain gilrs, updating its state and reporting connections.
    pub fn poll(&mut self) -> Vec<PadChange> {
        let mut out = Vec::new();
        while let Some(ev) = self.gilrs.as_mut().and_then(Gilrs::next_event) {
            match ev.event {
                gilrs::EventType::Connected => out.extend(self.added(ev.id)),
                gilrs::EventType::Disconnected => {
                    if let Some(i) = self.open.iter().position(|&p| p == ev.id) {
                        self.open.swap_remove(i);
                        out.push(PadChange::Removed { id: pad_id(ev.id) });
                    }
                }
                _ => {}
            }
        }
        out
    }

    /// SDL opened only devices it could map as a gamepad; gilrs also
    /// reports other evdev devices (a keyboard's media keys, say), so a
    /// device without a south face button is not one.
    fn added(&mut self, id: GamepadId) -> Option<PadChange> {
        let pad = self.gilrs.as_ref()?.connected_gamepad(id)?;
        if self.open.contains(&id) || pad.button_code(Button::South).is_none() {
            return None;
        }
        let name = Some(pad.name().to_owned())
            .filter(|n| !n.is_empty())
            .unwrap_or_else(|| "gamepad".to_owned());
        self.open.push(id);
        Some(PadChange::Added {
            id: pad_id(id),
            name,
        })
    }

    pub fn sample(&self) -> Vec<PadSample> {
        let Some(g) = &self.gilrs else {
            return Vec::new();
        };
        self.open
            .iter()
            .filter_map(|&id| g.connected_gamepad(id))
            .map(|p| {
                let axis = |a| f64::from(p.value(a));
                let trigger = |b, a| {
                    p.button_data(b)
                        .map(|d| f64::from(d.value()))
                        .unwrap_or_else(|| axis(a).max(0.0))
                };
                // gilrs has +y up; SDL, and so offler, has +y down.
                let down = |v: f64| -v + 0.0;
                let buttons = BUTTON_BITS
                    .iter()
                    .enumerate()
                    .filter(|&(_, &b)| p.is_pressed(b))
                    .fold(0, |m, (bit, _)| m | 1 << bit);
                let name = p.name();
                PadSample {
                    id: pad_id(p.id()),
                    axes: [
                        axis(Axis::LeftStickX),
                        down(axis(Axis::LeftStickY)),
                        axis(Axis::RightStickX),
                        down(axis(Axis::RightStickY)),
                        trigger(Button::LeftTrigger2, Axis::LeftZ),
                        trigger(Button::RightTrigger2, Axis::RightZ),
                    ],
                    buttons,
                    name: if name.is_empty() { "gamepad" } else { name }.to_owned(),
                }
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pad(id: i64, name: &str) -> PadSample {
        PadSample {
            id,
            axes: [0.5, -0.25, 0.0, 1.0, 0.0, 0.12345],
            buttons: 0b101,
            name: name.to_owned(),
        }
    }

    #[test]
    fn empty_is_empty() {
        assert_eq!(format_pads(&[]), "");
    }

    #[test]
    fn one_record_in_the_wire_form() {
        assert_eq!(
            format_pads(&[pad(3, "Pad, Deluxe")]),
            "3,0.5000,-0.2500,0.0000,1.0000,0.0000,0.1235,5,Pad, Deluxe"
        );
    }

    #[test]
    fn records_are_semicolon_joined_and_names_scrubbed() {
        assert_eq!(
            format_pads(&[pad(1, "a;b"), pad(2, "c")]),
            "1,0.5000,-0.2500,0.0000,1.0000,0.0000,0.1235,5,a b;\
             2,0.5000,-0.2500,0.0000,1.0000,0.0000,0.1235,5,c"
        );
    }

    #[test]
    fn axes_clamp_below_minus_one_only() {
        let mut p = pad(0, "x");
        p.axes = [-1.5, 1.5, -1.0, 0.0, 0.0, 0.0];
        assert!(format_pads(&[p]).starts_with("0,-1.0000,1.5000,-1.0000,"));
    }

    #[test]
    fn names_are_cut_to_95_bytes() {
        let s = format_pads(&[pad(0, &"n".repeat(200))]);
        assert_eq!(s.rsplit(',').next().unwrap().len(), 95);
    }

    #[test]
    fn output_stops_short_of_the_c_buffer() {
        let pads: Vec<_> = (0..100).map(|i| pad(i, &"n".repeat(95))).collect();
        let s = format_pads(&pads);
        assert!(s.len() < SPEC_BYTES);
        let n = s.split(';').count();
        assert!(n < 100 && n > 20);
    }

    #[test]
    fn flipped_zero_is_positive() {
        let down = |v: f64| -v + 0.0;
        assert_eq!(format!("{:.4}", down(0.0)), "0.0000");
    }
}
