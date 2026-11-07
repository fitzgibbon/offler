//! Key names. The C shim reported `SDL_GetKeyName` of the unshifted
//! keycode, translated to the browser's `KeyboardEvent.key` vocabulary; this
//! produces SDL's name from winit's key first, then applies the very same
//! translation, so scenes keep matching on one spelling.

use winit::keyboard::{Key, KeyCode, NamedKey, PhysicalKey};

/// The C copied names into a 64-byte buffer.
const KEY_BYTES: usize = 63;

/// `SDL_GetKeyName` for a key without modifiers. Keypad and modifier keys
/// are named from the physical key, as SDL names them by scancode.
pub fn sdl_name(logical: &Key, physical: PhysicalKey) -> String {
    if let PhysicalKey::Code(code) = physical
        && let Some(n) = scancode_name(code)
    {
        return n.to_owned();
    }
    match logical {
        Key::Character(s) => {
            let mut cs = s.chars();
            match (cs.next(), cs.next()) {
                (Some(c), None) if c.is_ascii_lowercase() => c.to_ascii_uppercase().to_string(),
                _ => s.to_string(),
            }
        }
        Key::Named(n) => named(*n),
        Key::Dead(Some(c)) => c.to_string(),
        Key::Dead(None) | Key::Unidentified(_) => String::new(),
    }
}

/// The C's `set_key`: arrows, space and return get their browser names, a
/// single upper-case letter is lowered, and the result is cut to fit.
pub fn browser_key(sdl: &str) -> String {
    let n = match sdl {
        "Left" => "ArrowLeft",
        "Right" => "ArrowRight",
        "Up" => "ArrowUp",
        "Down" => "ArrowDown",
        "Space" => " ",
        "Return" => "Enter",
        other => other,
    };
    let mut s = truncate(n, KEY_BYTES).to_owned();
    if s.len() == 1 && s.as_bytes()[0].is_ascii_uppercase() {
        s.make_ascii_lowercase();
    }
    s
}

/// Cut to at most `max` bytes on a character boundary.
pub fn truncate(s: &str, max: usize) -> &str {
    if s.len() <= max {
        return s;
    }
    let mut end = max;
    while !s.is_char_boundary(end) {
        end -= 1;
    }
    &s[..end]
}

fn scancode_name(code: KeyCode) -> Option<&'static str> {
    use KeyCode::*;
    Some(match code {
        Numpad0 => "Keypad 0",
        Numpad1 => "Keypad 1",
        Numpad2 => "Keypad 2",
        Numpad3 => "Keypad 3",
        Numpad4 => "Keypad 4",
        Numpad5 => "Keypad 5",
        Numpad6 => "Keypad 6",
        Numpad7 => "Keypad 7",
        Numpad8 => "Keypad 8",
        Numpad9 => "Keypad 9",
        NumpadAdd => "Keypad +",
        NumpadSubtract => "Keypad -",
        NumpadMultiply => "Keypad *",
        NumpadDivide => "Keypad /",
        NumpadDecimal => "Keypad .",
        NumpadComma => "Keypad ,",
        NumpadEnter => "Keypad Enter",
        NumpadEqual => "Keypad =",
        ShiftLeft => "Left Shift",
        ShiftRight => "Right Shift",
        ControlLeft => "Left Ctrl",
        ControlRight => "Right Ctrl",
        AltLeft => "Left Alt",
        AltRight => "Right Alt",
        SuperLeft => "Left GUI",
        SuperRight => "Right GUI",
        _ => return None,
    })
}

fn named(n: NamedKey) -> String {
    use NamedKey::*;
    let s = match n {
        Enter => "Return",
        Escape => "Escape",
        Backspace => "Backspace",
        Tab => "Tab",
        Space => "Space",
        Delete => "Delete",
        Insert => "Insert",
        Home => "Home",
        End => "End",
        PageUp => "PageUp",
        PageDown => "PageDown",
        ArrowLeft => "Left",
        ArrowRight => "Right",
        ArrowUp => "Up",
        ArrowDown => "Down",
        CapsLock => "CapsLock",
        NumLock => "Numlock",
        ScrollLock => "ScrollLock",
        PrintScreen => "PrintScreen",
        Pause => "Pause",
        ContextMenu => "Menu",
        Help => "Help",
        Select => "Select",
        Execute => "Execute",
        Again => "Again",
        Undo => "Undo",
        Cut => "Cut",
        Copy => "Copy",
        Paste => "Paste",
        Find => "Find",
        AudioVolumeMute => "Mute",
        AudioVolumeUp => "VolumeUp",
        AudioVolumeDown => "VolumeDown",
        MediaPlay => "MediaPlay",
        MediaPause => "MediaPause",
        MediaPlayPause => "MediaPlayPause",
        MediaStop => "MediaStop",
        MediaRecord => "MediaRecord",
        MediaFastForward => "MediaFastForward",
        MediaRewind => "MediaRewind",
        MediaTrackNext => "MediaNextTrack",
        MediaTrackPrevious => "MediaPreviousTrack",
        Eject => "Eject",
        Power => "Power",
        Shift => "Left Shift",
        Control => "Left Ctrl",
        Alt => "Left Alt",
        Super | Meta => "Left GUI",
        AltGraph => "Right Alt",
        ModeChange => "ModeSwitch",
        F1 => "F1",
        F2 => "F2",
        F3 => "F3",
        F4 => "F4",
        F5 => "F5",
        F6 => "F6",
        F7 => "F7",
        F8 => "F8",
        F9 => "F9",
        F10 => "F10",
        F11 => "F11",
        F12 => "F12",
        F13 => "F13",
        F14 => "F14",
        F15 => "F15",
        F16 => "F16",
        F17 => "F17",
        F18 => "F18",
        F19 => "F19",
        F20 => "F20",
        F21 => "F21",
        F22 => "F22",
        F23 => "F23",
        F24 => "F24",
        other => return format!("{other:?}"),
    };
    s.to_owned()
}

#[cfg(test)]
mod tests {
    use super::*;
    use winit::keyboard::NativeKeyCode;

    fn name(k: Key, code: KeyCode) -> String {
        browser_key(&sdl_name(&k, PhysicalKey::Code(code)))
    }

    #[test]
    fn letters_are_lower_case() {
        assert_eq!(name(Key::Character("a".into()), KeyCode::KeyA), "a");
        assert_eq!(name(Key::Character("Q".into()), KeyCode::KeyQ), "q");
        assert_eq!(
            sdl_name(
                &Key::Character("w".into()),
                PhysicalKey::Code(KeyCode::KeyW)
            ),
            "W"
        );
    }

    #[test]
    fn printable_symbols_keep_their_character() {
        assert_eq!(name(Key::Character("1".into()), KeyCode::Digit1), "1");
        assert_eq!(name(Key::Character("/".into()), KeyCode::Slash), "/");
        assert_eq!(name(Key::Character("é".into()), KeyCode::Digit2), "é");
    }

    #[test]
    fn named_keys_use_the_browser_vocabulary() {
        let n = |k| name(Key::Named(k), KeyCode::Fn);
        assert_eq!(n(NamedKey::ArrowLeft), "ArrowLeft");
        assert_eq!(n(NamedKey::ArrowRight), "ArrowRight");
        assert_eq!(n(NamedKey::ArrowUp), "ArrowUp");
        assert_eq!(n(NamedKey::ArrowDown), "ArrowDown");
        assert_eq!(n(NamedKey::Space), " ");
        assert_eq!(n(NamedKey::Enter), "Enter");
        assert_eq!(n(NamedKey::Escape), "Escape");
        assert_eq!(n(NamedKey::Tab), "Tab");
        assert_eq!(n(NamedKey::F5), "F5");
        assert_eq!(n(NamedKey::Backspace), "Backspace");
    }

    #[test]
    fn modifiers_and_keypad_are_named_by_position() {
        assert_eq!(
            name(Key::Named(NamedKey::Shift), KeyCode::ShiftLeft),
            "Left Shift"
        );
        assert_eq!(
            name(Key::Named(NamedKey::Shift), KeyCode::ShiftRight),
            "Right Shift"
        );
        assert_eq!(
            name(Key::Named(NamedKey::Control), KeyCode::ControlRight),
            "Right Ctrl"
        );
        assert_eq!(
            name(Key::Character("7".into()), KeyCode::Numpad7),
            "Keypad 7"
        );
        assert_eq!(
            name(Key::Named(NamedKey::Enter), KeyCode::NumpadEnter),
            "Keypad Enter"
        );
    }

    #[test]
    fn unknown_keys_are_empty() {
        let k = Key::Unidentified(winit::keyboard::NativeKey::Unidentified);
        assert_eq!(
            browser_key(&sdl_name(
                &k,
                PhysicalKey::Unidentified(NativeKeyCode::Unidentified)
            )),
            ""
        );
    }

    #[test]
    fn long_names_are_cut_on_a_character_boundary() {
        let long = "é".repeat(40);
        let k = browser_key(&long);
        assert_eq!(k.len(), 62);
        assert!(k.chars().all(|c| c == 'é'));
        assert_eq!(truncate("abc", 2), "ab");
    }
}
