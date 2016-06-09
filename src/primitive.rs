//! The geometric primitives used in SVG: moves, lines, bezier curves and arcs.
//!
//! blub

use path::{PathSeg, PathSegReader, PathSegToPrimitive, Error};
use std::fmt;
use std::fmt::Display;
use std::str::FromStr;
use std::ops::{Add, Sub};

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


/// Parses `Primitive`s from a path string.
pub struct PathReader<'a, T> {
    path_seg_reader: PathSegReader<'a, T>,
    path_seg_to_primitive: PathSegToPrimitive<T>,
}

impl<'a, T: Copy + FromStr + Add<T, Output=T> + Sub<T, Output=T> + Default> PathReader<'a, T> {
    /// Creates a new `PathReader` using for instance a `&str` or a `&[u8]`.
    pub fn new<B: AsRef<[u8]> + ?Sized>(src: &'a B) -> PathReader<'a, T> {
        PathReader {
            path_seg_reader: PathSegReader::new(src),
            path_seg_to_primitive: PathSegToPrimitive::new(),
        }
    }

    /// Parses the first `Primitive` and removes it from the source.
    ///
    /// Returns `None` when the source doesn't contain any path segments
    /// anymore (empty or only whitespace).
    /// Otherwise it returns either the `Primitive` or an `Error` if one
    /// occured.
    pub fn pop_one_pathseg(&mut self) -> Option<Result<Primitive<T>, Error>> {
        match self.path_seg_reader.pop_one_pathseg() {
            None => None,
            Some(result) => {
                Some(match result {
                    Err(e) => Err(e),
                    Ok(pathseg) => Ok(self.path_seg_to_primitive.convert(pathseg))
                })
            }
        }
    }

    /// Returns the maximum precision encountered so far.
    pub fn precision(&self) -> usize {
        self.path_seg_reader.precision()
    }
}

/// Parse all `Primitive`s from a path and put them in a newly allocated array.
///
/// Returns all path segments as `Primitive`s that were parsed until an error occurred or the string was empty,
/// the error if one occured and the maximum precision.
pub fn parse_path<'a, T: Copy + FromStr + Add<T, Output=T> + Sub<T, Output=T> + Default, B: AsRef<[u8]> + ?Sized>(src: &'a B) -> (Vec<Primitive<T>>, Option<Error>, usize) {
    let mut parser = PathReader::new(src);
    let mut array = Vec::new();
    loop {
        let result = match parser.pop_one_pathseg() {
            Some(result) => result,
            None => break
        };
        match result {
            Ok(seg) => { array.push(seg) }
            Err(e) => { return (array, Some(e), parser.precision()) }
        }
        
    }
    (array, None, parser.precision())
}

/// The `Iterator` for `PathReader`.
pub struct PathPrimitives<'a, T> {
    reader: PathReader<'a, T>,
    done: bool,
}

impl<'a, T: Copy + FromStr + Add<T, Output=T> + Sub<T, Output=T> + Default> Iterator for PathPrimitives<'a, T> {
    type Item = Result<Primitive<T>, Error>;

    fn next(&mut self) -> Option<Result<Primitive<T>, Error>> {
        if self.done {
            return None;
        }

        let next = self.reader.pop_one_pathseg();

        // Don't emit anything after an error occured.
        if let Some(Err(_)) = next {
            self.done = true;
        }
        
        next
    }
}

impl<'a, T> From<PathReader<'a, T>> for PathPrimitives<'a, T> {
    fn from(reader: PathReader<'a, T>) -> PathPrimitives<'a, T> {
        PathPrimitives {
            reader: reader,
            done: false,
        }
    }
}

impl<'a, T: Copy + FromStr + Add<T, Output=T> + Sub<T, Output=T> + Default> IntoIterator for PathReader<'a, T> {
    type Item = Result<Primitive<T>, Error>;
    type IntoIter = PathPrimitives<'a, T>;

    fn into_iter(self) -> PathPrimitives<'a, T> {
        self.into()
    }
}