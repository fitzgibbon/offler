//! Flat numeric arrays behind `Offler.Gfx.Array` and `Offler.Ecs.Column`.
//!
//! The C shim handed out `calloc`'d memory; here each array is a zeroed
//! `Vec` behind a `RefCell`, since Idris holds handles shared. Positions are
//! proved or tested on the Idris side, so an out-of-range index is a bug
//! there: it panics here where C would have scribbled.

use std::cell::{Ref, RefCell};

/// A zeroed buffer of `T`, mutated through a shared handle.
pub struct Array<T>(RefCell<Vec<T>>);

pub type F32Array = Array<f32>;
pub type U32Array = Array<u32>;
pub type I32Array = Array<i32>;

impl<T: Copy + Default> Array<T> {
    fn zeroed(n: usize) -> Self {
        Self(RefCell::new(vec![T::default(); n]))
    }

    fn set(&self, i: i64, v: T) {
        self.0.borrow_mut()[index(i)] = v;
    }

    fn get(&self, i: i64) -> T {
        self.0.borrow()[index(i)]
    }

    pub fn borrow(&self) -> Ref<'_, Vec<T>> {
        self.0.borrow()
    }

    /// The first `n` elements, clamped to the array, as the uploads read them.
    pub fn prefix(&self, n: usize) -> Ref<'_, [T]> {
        Ref::map(self.0.borrow(), |v| &v[..n.min(v.len())])
    }
}

fn index(i: i64) -> usize {
    usize::try_from(i).expect("offler: negative array index")
}

/// A length from Idris: negative counts as empty, as `calloc` of a wrapped
/// size could never have been relied on.
pub(crate) fn count(n: i64) -> usize {
    usize::try_from(n).unwrap_or(0)
}

/// `(float)v`: the C conversion from a double argument.
fn narrow(v: f64) -> f32 {
    v as f32
}

pub fn offler_f32_new(n: i64) -> F32Array {
    Array::zeroed(count(n))
}

pub fn offler_f32_poke(a: &F32Array, i: i64, v: f64) {
    a.set(i, narrow(v));
}

pub fn offler_f32_peek(a: &F32Array, i: i64) -> f64 {
    f64::from(a.get(i))
}

pub fn offler_f32_poke4(a: &F32Array, o: i64, x: f64, y: f64, z: f64, w: f64) {
    poke_run(a, o, &[x, y, z, w]);
}

#[allow(clippy::too_many_arguments)]
pub fn offler_f32_poke16(
    a: &F32Array,
    o: i64,
    x0: f64,
    x1: f64,
    x2: f64,
    x3: f64,
    x4: f64,
    x5: f64,
    x6: f64,
    x7: f64,
    x8: f64,
    x9: f64,
    x10: f64,
    x11: f64,
    x12: f64,
    x13: f64,
    x14: f64,
    x15: f64,
) {
    poke_run(
        a,
        o,
        &[
            x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15,
        ],
    );
}

fn poke_run(a: &F32Array, o: i64, xs: &[f64]) {
    let o = index(o);
    let mut v = a.0.borrow_mut();
    for (d, &x) in v[o..o + xs.len()].iter_mut().zip(xs) {
        *d = narrow(x);
    }
}

/// Move `n` floats within one array, overlapping allowed: `memmove`.
pub fn offler_f32_move(a: &F32Array, dst: i64, src: i64, n: i64) {
    let n = count(n);
    if n == 0 {
        return;
    }
    let src = index(src);
    a.0.borrow_mut().copy_within(src..src + n, index(dst));
}

pub fn offler_u32_new(n: i64) -> U32Array {
    Array::zeroed(count(n))
}

/// `(unsigned)v` of a C `int`: the low 32 bits.
pub fn offler_u32_poke(a: &U32Array, i: i64, v: i64) {
    a.set(i, v as u32);
}

/// At least one element, as the C allocated, so an empty column is still a
/// valid handle.
pub fn offler_i32_new(n: i64) -> I32Array {
    Array::zeroed(count(n).max(1))
}

pub fn offler_i32_peek(a: &I32Array, i: i64) -> i64 {
    i64::from(a.get(i))
}

/// The low 32 bits, as the C `int` parameter received them.
pub fn offler_i32_poke(a: &I32Array, i: i64, v: i64) {
    a.set(i, v as i32);
}

/// Copy the first `n` entries of `src` over the start of `dst`. The two may
/// be the same handle, which `memcpy` would have left undefined; here it is
/// a no-op copy.
pub fn offler_i32_copy(dst: &I32Array, src: &I32Array, n: i64) {
    let n = count(n);
    if n == 0 || std::ptr::eq(dst, src) {
        return;
    }
    dst.0.borrow_mut()[..n].copy_from_slice(&src.0.borrow()[..n]);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_arrays_are_zeroed() {
        let a = offler_f32_new(5);
        assert_eq!(*a.borrow(), vec![0.0; 5]);
        assert_eq!(offler_u32_new(3).borrow().len(), 3);
        assert_eq!(offler_f32_new(-2).borrow().len(), 0);
    }

    #[test]
    fn i32_columns_have_at_least_one_slot() {
        assert_eq!(offler_i32_new(0).borrow().len(), 1);
        assert_eq!(offler_i32_new(-4).borrow().len(), 1);
        assert_eq!(offler_i32_new(7).borrow().len(), 7);
    }

    #[test]
    fn f32_poke_narrows_and_peek_widens() {
        let a = offler_f32_new(2);
        offler_f32_poke(&a, 1, 0.1);
        assert_eq!(offler_f32_peek(&a, 1), f64::from(0.1f32));
        assert_eq!(offler_f32_peek(&a, 0), 0.0);
    }

    #[test]
    fn poke4_and_poke16_write_runs_at_an_offset() {
        let a = offler_f32_new(24);
        offler_f32_poke4(&a, 2, 1.0, 2.0, 3.0, 4.0);
        assert_eq!(&a.borrow()[..7], &[0.0, 0.0, 1.0, 2.0, 3.0, 4.0, 0.0]);
        let xs: Vec<f64> = (0..16).map(f64::from).collect();
        offler_f32_poke16(
            &a, 8, xs[0], xs[1], xs[2], xs[3], xs[4], xs[5], xs[6], xs[7], xs[8], xs[9], xs[10],
            xs[11], xs[12], xs[13], xs[14], xs[15],
        );
        let got: Vec<f64> = a.borrow()[8..24].iter().map(|&x| f64::from(x)).collect();
        assert_eq!(got, xs);
        assert_eq!(a.borrow()[7], 0.0);
    }

    #[test]
    #[should_panic]
    fn out_of_range_writes_panic() {
        let a = offler_f32_new(4);
        offler_f32_poke4(&a, 1, 1.0, 1.0, 1.0, 1.0);
    }

    #[test]
    fn u32_and_i32_take_the_low_bits() {
        let u = offler_u32_new(2);
        offler_u32_poke(&u, 0, -1);
        offler_u32_poke(&u, 1, 7);
        assert_eq!(*u.borrow(), vec![u32::MAX, 7]);
        let i = offler_i32_new(1);
        offler_i32_poke(&i, 0, (1 << 32) + 5);
        assert_eq!(offler_i32_peek(&i, 0), 5);
        offler_i32_poke(&i, 0, -3);
        assert_eq!(offler_i32_peek(&i, 0), -3);
    }

    #[test]
    fn i32_copy_copies_a_prefix() {
        let s = offler_i32_new(4);
        let d = offler_i32_new(4);
        for k in 0..4 {
            offler_i32_poke(&s, k, k + 10);
            offler_i32_poke(&d, k, -1);
        }
        offler_i32_copy(&d, &s, 3);
        assert_eq!(*d.borrow(), vec![10, 11, 12, -1]);
        offler_i32_copy(&d, &d, 2);
        offler_i32_copy(&d, &s, 0);
        assert_eq!(*d.borrow(), vec![10, 11, 12, -1]);
    }

    #[test]
    fn f32_move_handles_overlap_both_ways() {
        let a = offler_f32_new(6);
        for k in 0..6 {
            offler_f32_poke(&a, k, k as f64);
        }
        offler_f32_move(&a, 1, 0, 3);
        assert_eq!(*a.borrow(), vec![0.0, 0.0, 1.0, 2.0, 4.0, 5.0]);
        offler_f32_move(&a, 0, 2, 4);
        assert_eq!(*a.borrow(), vec![1.0, 2.0, 4.0, 5.0, 4.0, 5.0]);
        offler_f32_move(&a, 0, 5, 0);
        assert_eq!(a.borrow()[0], 1.0);
    }

    #[test]
    fn prefix_clamps_to_the_array() {
        let a = offler_f32_new(3);
        assert_eq!(a.prefix(2).len(), 2);
        assert_eq!(a.prefix(10).len(), 3);
    }
}
