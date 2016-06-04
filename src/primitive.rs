//! The geometric primitives used in SVG: moves, lines, bezier curves and arcs.
//!
//! blub

use path::PathSeg;
use std::fmt;
use std::fmt::Display;

/// A geometric primitive.
///
/// The geometric primitives that can represent all geometries supported by SVG.
/// Those are essentially absolute version of `PathSeg`s, without all the special cases like horizontal lines.
#[derive(Eq, PartialEq, Copy, Clone)]
pub enum Primitive<T> {
    Closepath,
    Moveto {
        p: (T, T),
    },
    Lineto {
        p: (T, T),
    },
    CurvetoCubic {
        p1: (T, T),
        p2: (T, T),
        p: (T, T),
    },
    CurvetoQuadratic {
        p1: (T, T),
        p: (T, T),
    },
    Arc {
        r1: T,
        r2: T,
        rotation: T,
        large_arc_flag: bool,
        sweep_flag: bool,
        p: (T, T),
    },
}

impl<T: Display + Copy> fmt::Debug for Primitive<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let primitive = self.clone();
        let seg : PathSeg<T> = primitive.into();
        seg.fmt(f)
    }
}