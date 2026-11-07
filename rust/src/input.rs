//! Window input: winit events buffered as offler's events, and the
//! one-event-per-call reporting protocol `offler_poll` speaks.
//!
//! Codes: 1 close, 2 resize, 3 keydown, 4 keyup, 5 move, 6 down, 7 up,
//! 8 wheel, 9 pointer delta, 10 finger down, 11 finger motion, 12 finger
//! up, 13 gamepad added, 14 gamepad removed, 15 pointer-lock change.

use std::collections::VecDeque;
use std::sync::Arc;

use winit::application::ApplicationHandler;
use winit::dpi::{LogicalSize, PhysicalPosition};
use winit::event::{
    DeviceEvent, DeviceId, ElementState, MouseButton, MouseScrollDelta, TouchPhase, WindowEvent,
};
use winit::event_loop::ActiveEventLoop;
use winit::window::{Window, WindowId};

use crate::keys;

/// An event code, as `Offler.Native.Platform.drain` reads it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(i64)]
pub enum Code {
    Empty = 0,
    Close = 1,
    Resize = 2,
    KeyDown = 3,
    KeyUp = 4,
    Move = 5,
    Down = 6,
    Up = 7,
    Wheel = 8,
    Delta = 9,
    FingerDown = 10,
    FingerMotion = 11,
    FingerUp = 12,
    PadAdded = 13,
    PadRemoved = 14,
    LockChange = 15,
}

/// One buffered event, coordinates already in drawing-buffer pixels.
#[derive(Clone, Debug, PartialEq)]
pub enum Input {
    Close,
    Resize,
    Key {
        down: bool,
        name: String,
    },
    /// An absolute move, which also reports its delta as a second event.
    Move {
        x: f64,
        y: f64,
        dx: f64,
        dy: f64,
    },
    /// A relative move alone, under pointer lock.
    Delta {
        dx: f64,
        dy: f64,
    },
    Button {
        down: bool,
        button: i64,
        x: f64,
        y: f64,
    },
    Wheel(f64),
    Finger {
        code: Code,
        id: i64,
        x: f64,
        y: f64,
    },
    PadAdded {
        id: i64,
        name: String,
    },
    PadRemoved {
        id: i64,
    },
}

/// The event being reported, refreshed by each poll.
#[derive(Debug, Default)]
pub struct Report {
    pub key: String,
    pub x: f64,
    pub y: f64,
    pub wheel: f64,
    pub dx: f64,
    pub dy: f64,
    pub button: i64,
    pub id: i64,
    /// The second code of a one-event pair (move then delta).
    pending: Option<Code>,
    /// A pointer-lock change to report: `Some(true)` acquired.
    lock_change: Option<bool>,
}

impl Report {
    pub fn lock_changed(&mut self, acquired: bool) {
        self.lock_change = Some(acquired);
    }

    /// The C's `offler_poll` order: a parked second code, then a lock
    /// change, then the next queued event.
    pub fn next(&mut self, queue: &mut VecDeque<Input>) -> Code {
        if let Some(c) = self.pending.take() {
            return c;
        }
        if let Some(on) = self.lock_change.take() {
            self.id = i64::from(on);
            return Code::LockChange;
        }
        let Some(ev) = queue.pop_front() else {
            return Code::Empty;
        };
        match ev {
            Input::Close => Code::Close,
            Input::Resize => Code::Resize,
            Input::Key { down, name } => {
                self.key = name;
                if down { Code::KeyDown } else { Code::KeyUp }
            }
            Input::Move { x, y, dx, dy } => {
                (self.dx, self.dy, self.x, self.y) = (dx, dy, x, y);
                self.pending = Some(Code::Delta);
                Code::Move
            }
            Input::Delta { dx, dy } => {
                (self.dx, self.dy) = (dx, dy);
                Code::Delta
            }
            Input::Button { down, button, x, y } => {
                (self.x, self.y, self.button) = (x, y, button);
                if down { Code::Down } else { Code::Up }
            }
            Input::Wheel(w) => {
                self.wheel = w;
                Code::Wheel
            }
            Input::Finger { code, id, x, y } => {
                (self.id, self.x, self.y) = (id, x, y);
                code
            }
            Input::PadAdded { id, name } => {
                self.key = keys::truncate(&name, 63).to_owned();
                self.id = id;
                Code::PadAdded
            }
            Input::PadRemoved { id } => {
                self.id = id;
                Code::PadRemoved
            }
        }
    }
}

/// SDL's button numbering, with left, middle and right as the browser's.
pub fn button_code(b: MouseButton) -> i64 {
    match b {
        MouseButton::Left => 0,
        MouseButton::Middle => 1,
        MouseButton::Right => 2,
        MouseButton::Back => 4,
        MouseButton::Forward => 5,
        MouseButton::Other(n) => i64::from(n),
    }
}

/// Scroll in wheel notches, +y away from the user. Pixel deltas (touchpads)
/// are taken at ten pixels a notch, the Wayland convention SDL follows.
pub fn wheel_notches(d: MouseScrollDelta) -> f64 {
    match d {
        MouseScrollDelta::LineDelta(_, y) => f64::from(y),
        MouseScrollDelta::PixelDelta(p) => p.y / 10.0,
    }
}

pub fn finger_code(p: TouchPhase) -> Code {
    match p {
        TouchPhase::Started => Code::FingerDown,
        TouchPhase::Moved => Code::FingerMotion,
        TouchPhase::Ended | TouchPhase::Cancelled => Code::FingerUp,
    }
}

/// Collects winit's events while the event loop is pumped.
pub struct Collector {
    pub title: String,
    pub window: Option<Arc<Window>>,
    pub create_error: Option<String>,
    pub queue: VecDeque<Input>,
    /// The drawing-buffer size the surface was last configured to.
    pub surface: (u32, u32),
    /// Under pointer lock only relative motion is reported.
    pub relative: bool,
    /// The last absolute pointer position, in window pixels.
    cursor: Option<PhysicalPosition<f64>>,
}

impl Collector {
    pub fn new(title: &str) -> Self {
        Self {
            title: title.to_owned(),
            window: None,
            create_error: None,
            queue: VecDeque::new(),
            surface: (1, 1),
            relative: false,
            cursor: None,
        }
    }

    /// Window pixels to drawing-buffer pixels, as the C scaled SDL's.
    fn scale(&self) -> (f64, f64) {
        let Some(w) = &self.window else {
            return (1.0, 1.0);
        };
        let s = w.inner_size();
        (
            f64::from(self.surface.0) / f64::from(s.width.max(1)),
            f64::from(self.surface.1) / f64::from(s.height.max(1)),
        )
    }

    fn pointer(&self) -> (f64, f64) {
        let (sx, sy) = self.scale();
        self.cursor.map_or((0.0, 0.0), |p| (p.x * sx, p.y * sy))
    }
}

impl ApplicationHandler for Collector {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        if self.window.is_some() || self.create_error.is_some() {
            return;
        }
        let attrs = Window::default_attributes()
            .with_title(self.title.clone())
            .with_inner_size(LogicalSize::new(1280.0, 800.0))
            .with_resizable(true);
        match event_loop.create_window(attrs) {
            Ok(w) => self.window = Some(Arc::new(w)),
            Err(e) => self.create_error = Some(e.to_string()),
        }
    }

    fn window_event(&mut self, _: &ActiveEventLoop, _: WindowId, event: WindowEvent) {
        let ev = match event {
            WindowEvent::CloseRequested | WindowEvent::Destroyed => Input::Close,
            WindowEvent::Resized(_) => Input::Resize,
            WindowEvent::KeyboardInput {
                event,
                is_synthetic,
                ..
            } => {
                let down = event.state == ElementState::Pressed;
                // Keys held as focus arrives are not SDL key presses.
                if down && is_synthetic {
                    return;
                }
                Input::Key {
                    down,
                    name: keys::browser_key(&keys::sdl_name(
                        &unmodified(&event),
                        event.physical_key,
                    )),
                }
            }
            WindowEvent::CursorMoved { position, .. } => {
                let last = self.cursor.replace(position);
                if self.relative {
                    return;
                }
                let (sx, sy) = self.scale();
                let (dx, dy) = last.map_or((0.0, 0.0), |l| (position.x - l.x, position.y - l.y));
                Input::Move {
                    x: position.x * sx,
                    y: position.y * sy,
                    dx: dx * sx,
                    dy: dy * sy,
                }
            }
            WindowEvent::MouseInput { state, button, .. } => {
                let (x, y) = self.pointer();
                Input::Button {
                    down: state == ElementState::Pressed,
                    button: button_code(button),
                    x,
                    y,
                }
            }
            WindowEvent::MouseWheel { delta, .. } => Input::Wheel(wheel_notches(delta)),
            WindowEvent::Touch(t) => {
                let (sx, sy) = self.scale();
                Input::Finger {
                    code: finger_code(t.phase),
                    id: i64::from(t.id as i32),
                    x: t.location.x * sx,
                    y: t.location.y * sy,
                }
            }
            _ => return,
        };
        self.queue.push_back(ev);
    }

    fn device_event(&mut self, _: &ActiveEventLoop, _: DeviceId, event: DeviceEvent) {
        if let DeviceEvent::MouseMotion { delta: (dx, dy) } = event
            && self.relative
        {
            self.queue.push_back(Input::Delta { dx, dy });
        }
    }
}

/// The key as typed without modifiers, as SDL's keycode is.
#[cfg(any(
    target_os = "windows",
    target_os = "macos",
    target_os = "linux",
    target_os = "freebsd",
    target_os = "dragonfly",
    target_os = "netbsd",
    target_os = "openbsd"
))]
fn unmodified(e: &winit::event::KeyEvent) -> winit::keyboard::Key {
    use winit::platform::modifier_supplement::KeyEventExtModifierSupplement;
    e.key_without_modifiers()
}

#[cfg(not(any(
    target_os = "windows",
    target_os = "macos",
    target_os = "linux",
    target_os = "freebsd",
    target_os = "dragonfly",
    target_os = "netbsd",
    target_os = "openbsd"
)))]
fn unmodified(e: &winit::event::KeyEvent) -> winit::keyboard::Key {
    e.logical_key.clone()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_queue_reports_zero() {
        let mut r = Report::default();
        assert_eq!(r.next(&mut VecDeque::new()) as i64, 0);
    }

    #[test]
    fn codes_match_the_idris_drain() {
        let codes = [
            (Code::Close, 1),
            (Code::Resize, 2),
            (Code::KeyDown, 3),
            (Code::KeyUp, 4),
            (Code::Move, 5),
            (Code::Down, 6),
            (Code::Up, 7),
            (Code::Wheel, 8),
            (Code::Delta, 9),
            (Code::FingerDown, 10),
            (Code::FingerMotion, 11),
            (Code::FingerUp, 12),
            (Code::PadAdded, 13),
            (Code::PadRemoved, 14),
            (Code::LockChange, 15),
        ];
        for (c, n) in codes {
            assert_eq!(c as i64, n);
        }
    }

    #[test]
    fn a_move_reports_position_then_delta() {
        let mut r = Report::default();
        let mut q = VecDeque::from([
            Input::Move {
                x: 10.0,
                y: 20.0,
                dx: 1.0,
                dy: -2.0,
            },
            Input::Close,
        ]);
        assert_eq!(r.next(&mut q), Code::Move);
        assert_eq!((r.x, r.y), (10.0, 20.0));
        assert_eq!(r.next(&mut q), Code::Delta);
        assert_eq!((r.dx, r.dy), (1.0, -2.0));
        assert_eq!(r.next(&mut q), Code::Close);
        assert_eq!(r.next(&mut q), Code::Empty);
    }

    #[test]
    fn lock_changes_come_before_queued_events_and_after_a_pending_delta() {
        let mut r = Report::default();
        let mut q = VecDeque::from([
            Input::Move {
                x: 0.0,
                y: 0.0,
                dx: 0.0,
                dy: 0.0,
            },
            Input::Resize,
        ]);
        assert_eq!(r.next(&mut q), Code::Move);
        r.lock_changed(true);
        assert_eq!(r.next(&mut q), Code::Delta);
        assert_eq!(r.next(&mut q), Code::LockChange);
        assert_eq!(r.id, 1);
        r.lock_changed(false);
        assert_eq!(r.next(&mut q), Code::LockChange);
        assert_eq!(r.id, 0);
        assert_eq!(r.next(&mut q), Code::Resize);
    }

    #[test]
    fn keys_buttons_wheel_fingers_and_pads_fill_the_report() {
        let mut r = Report::default();
        let mut q = VecDeque::from([
            Input::Key {
                down: true,
                name: "a".into(),
            },
            Input::Key {
                down: false,
                name: "Escape".into(),
            },
            Input::Button {
                down: true,
                button: 2,
                x: 3.0,
                y: 4.0,
            },
            Input::Button {
                down: false,
                button: 0,
                x: 5.0,
                y: 6.0,
            },
            Input::Wheel(-1.0),
            Input::Delta { dx: 7.0, dy: 8.0 },
            Input::Finger {
                code: Code::FingerMotion,
                id: 9,
                x: 1.0,
                y: 2.0,
            },
            Input::PadAdded {
                id: 2,
                name: "x".repeat(80),
            },
            Input::PadRemoved { id: 2 },
        ]);
        assert_eq!(r.next(&mut q), Code::KeyDown);
        assert_eq!(r.key, "a");
        assert_eq!(r.next(&mut q), Code::KeyUp);
        assert_eq!(r.key, "Escape");
        assert_eq!(r.next(&mut q), Code::Down);
        assert_eq!((r.button, r.x, r.y), (2, 3.0, 4.0));
        assert_eq!(r.next(&mut q), Code::Up);
        assert_eq!(r.button, 0);
        assert_eq!(r.next(&mut q), Code::Wheel);
        assert_eq!(r.wheel, -1.0);
        assert_eq!(r.next(&mut q), Code::Delta);
        assert_eq!((r.dx, r.dy), (7.0, 8.0));
        assert_eq!(r.next(&mut q), Code::FingerMotion);
        assert_eq!((r.id, r.x, r.y), (9, 1.0, 2.0));
        assert_eq!(r.next(&mut q), Code::PadAdded);
        assert_eq!((r.id, r.key.len()), (2, 63));
        assert_eq!(r.next(&mut q), Code::PadRemoved);
        assert_eq!(r.next(&mut q), Code::Empty);
    }

    #[test]
    fn buttons_wheel_and_phases_translate() {
        assert_eq!(button_code(MouseButton::Left), 0);
        assert_eq!(button_code(MouseButton::Middle), 1);
        assert_eq!(button_code(MouseButton::Right), 2);
        assert_eq!(button_code(MouseButton::Back), 4);
        assert_eq!(button_code(MouseButton::Other(9)), 9);
        assert_eq!(wheel_notches(MouseScrollDelta::LineDelta(0.0, 1.0)), 1.0);
        assert_eq!(
            wheel_notches(MouseScrollDelta::PixelDelta(PhysicalPosition::new(
                0.0, -20.0
            ))),
            -2.0
        );
        assert_eq!(finger_code(TouchPhase::Started), Code::FingerDown);
        assert_eq!(finger_code(TouchPhase::Moved), Code::FingerMotion);
        assert_eq!(finger_code(TouchPhase::Ended), Code::FingerUp);
        assert_eq!(finger_code(TouchPhase::Cancelled), Code::FingerUp);
    }
}
