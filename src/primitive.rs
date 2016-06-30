//! The geometric primitives used in SVG: moves, lines, bezier curves and arcs.
//!
//! blub

use path::{PathSeg, PathSegReader, PathSegToPrimitive, Error, PathSegWriter};
use std::fmt;
use std::fmt::Display;
use std::str::FromStr;
use std::ops::{Add, Sub};

use num_traits::Zero;

/// A geometric primitive.
///
/// The geometric primitives that can represent all geometries supported by SVG.
/// Those are essentially absolute version of `PathSeg`s, without all the special cases like horizontal lines.
#[derive(Eq, PartialEq, Copy, Clone)]
pub enum Primitive<T> {
    Closepath,
    Moveto((T, T)),
    Lineto((T, T)),
    CurvetoCubic((T, T), (T, T), (T, T)),
    CurvetoQuadratic((T, T), (T, T)),
    Arc(T, T, T, bool, bool, (T,T)),
}

impl<T: Display + Copy> fmt::Debug for Primitive<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let primitive = self.clone();
        let seg : PathSeg<T> = primitive.into();
        seg.fmt(f)
    }
}

/// Parse a [`<line>`](http://www.w3.org/TR/SVG11/shapes.html#LineElement) into an array of primitives.
pub fn parse_line<T>(x1: T, y1: T, x2: T, y2: T) -> [Primitive<T>; 2] {
    [Primitive::Moveto((x1, y1)), Primitive::Lineto((x2, y2))]
}

/// Parse a [`<circle>`](http://www.w3.org/TR/SVG11/shapes.html#CircleElement) into an array of primitives.
///
/// Note that you need to check that r is positive.
pub fn parse_circle<T>(cx: T, cy: T, r: T) -> [Primitive<T>; 4]
where T: Copy + Add<T, Output=T> + Sub<T, Output=T> + Zero {
    [Primitive::Moveto((cx+r, cy)),
     Primitive::Arc(r, r, Zero::zero(), false, true, (cx-r, cy)),
     Primitive::Arc(r, r, Zero::zero(), false, true, (cx+r, cy)),
     Primitive::Closepath]
}

/// Parse an [`<ellipse>`](http://www.w3.org/TR/SVG11/shapes.html#EllipseElement) into an array of primitives.
///
/// Note that you need to check that `rx` and `ry` are positive.
pub fn parse_ellipse<T>(cx: T, cy: T, rx: T, ry: T) -> [Primitive<T>; 4]
where T: Copy + Add<T, Output=T> + Sub<T, Output=T> + Zero {
    [Primitive::Moveto((cx+rx, cy)),
     Primitive::Arc(rx, ry, Zero::zero(), false, true, (cx-rx, cy)),
     Primitive::Arc(rx, ry, Zero::zero(), false, true, (cx+rx, cy)),
     Primitive::Closepath]
}

/// Parse a [`<rect>`](http://www.w3.org/TR/SVG11/shapes.html#RectElement) without corners into an array of primitives.
///
/// Note that `width` and `height` need to be larger than zero according to the spec.
pub fn parse_simple_rect<T>(x: T, y: T, width: T, height: T) -> [Primitive<T>; 6]
where T: Copy + Add<T, Output=T> {
    [Primitive::Moveto((x, y)),
     Primitive::Lineto((x+width, y)),
     Primitive::Lineto((x+width, y+height)),
     Primitive::Lineto((x, y+height)),
     Primitive::Lineto((x, y)),
     Primitive::Closepath]
}

/// Parses `Primitive`s from a path string.
pub struct PathReader<'a, T> {
    path_seg_reader: PathSegReader<'a, T>,
    path_seg_to_primitive: PathSegToPrimitive<T>,
}

impl<'a, T: Copy + FromStr + Add<T, Output=T> + Sub<T, Output=T> + Zero> PathReader<'a, T> {
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

/// Parse all `Primitive`s from a [`<path>`](http://www.w3.org/TR/SVG11/paths.html#PathElement) and put them in a newly allocated array.
///
/// Returns all path segments as `Primitive`s that were parsed until an error occurred or the string was empty,
/// the error if one occured and the maximum precision.
pub fn parse_path<'a, T, B: ?Sized>(src: &'a B) -> (Vec<Primitive<T>>, Option<Error>, usize)
where T: Copy + FromStr + Add<T, Output=T> + Sub<T, Output=T> + Zero, B: AsRef<[u8]> {
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

impl<'a, T: Copy + FromStr + Add<T, Output=T> + Sub<T, Output=T> + Zero> Iterator for PathPrimitives<'a, T> {
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

impl<'a, T: Copy + FromStr + Add<T, Output=T> + Sub<T, Output=T> + Zero> IntoIterator for PathReader<'a, T> {
    type Item = Result<Primitive<T>, Error>;
    type IntoIter = PathPrimitives<'a, T>;

    fn into_iter(self) -> PathPrimitives<'a, T> {
        self.into()
    }
}

pub struct PathWriter<'a, W: 'a + fmt::Write, T> {
    psw: PathSegWriter<'a, W>,
    /// The position we're currently at.
    pos: (T,T),
    /// Where we moved to with the last move.
    last_move: (T,T),
}

impl <'a, W: 'a + fmt::Write, T: Zero> PathWriter<'a, W, T> {
    pub fn new(sink: &'a mut W, pretty: bool, precision: Option<usize>) -> PathWriter<'a, W, T> {
        PathWriter {
            psw: PathSegWriter::new(sink, pretty, precision),
            pos: (Zero::zero(), Zero::zero()),
            last_move: (Zero::zero(), Zero::zero()),
        }
    }
}

fn to_rel<T: Sub<T, Output=T>>(pos: (T,T), abs: (T, T)) -> (T, T) {
    let (abs_x, abs_y) = abs;
    let (pos_x, pos_y) = pos;
    (abs_x - pos_x, abs_y - pos_y)
}

impl <'a, W: 'a + fmt::Write, T: Copy + Add<T, Output=T> + Sub<T, Output=T> + Display> PathWriter<'a, W, T> {
    pub fn write(&mut self, primitive: Primitive<T>) -> Result<(), fmt::Error> {
        let path_segs = match primitive {
            Primitive::Closepath => {
                // Current position moves back on closepath.
                self.pos = self.last_move;
                let pathseg : PathSeg<T> = PathSeg::Closepath;
                return self.psw.write(pathseg);
            }
            Primitive::Moveto(p) => {
                let path_segs = [
                    PathSeg::MovetoAbs(p),
                    PathSeg::MovetoRel(to_rel(self.pos, p))
                ];
                self.pos = p;
                self.last_move = p;
                path_segs
            }
            Primitive::Lineto(p) => {
                let path_segs = [
                    PathSeg::LinetoAbs(p),
                    PathSeg::LinetoRel(to_rel(self.pos, p))
                ];
                self.pos = p;
                path_segs
            }
            Primitive::CurvetoCubic(p1, p2, p) => {
                let path_segs = [
                    PathSeg::CurvetoCubicAbs(p1, p2, p),
                    PathSeg::CurvetoCubicRel(to_rel(self.pos, p1), to_rel(self.pos, p2), to_rel(self.pos, p))
                ];
                self.pos = p;
                path_segs
            }
            Primitive::CurvetoQuadratic(p1, p) => {
                let path_segs = [
                    PathSeg::CurvetoQuadraticAbs(p1, p),
                    PathSeg::CurvetoQuadraticRel(to_rel(self.pos, p1), to_rel(self.pos, p))
                ];
                self.pos = p;
                path_segs
            }
            Primitive::Arc(r1, r2, rot, f1, f2, p) => {
                let path_segs = [
                    PathSeg::ArcAbs(r1, r2, rot, f1, f2, p),
                    PathSeg::ArcAbs(r1, r2, rot, f1, f2, to_rel(self.pos, p))
                ];
                self.pos = p;
                path_segs
            }
        };

        // Select the shorter path segment.
        // Prefer writing absolute path segments when both have the same lengh.
        // Absolute path segments have the advantage that they don't accumulate rounding errors.
        if self.psw.test_write(path_segs[0]) <= self.psw.test_write(path_segs[1]) {
            // Write absolute path segment.
            self.psw.write(path_segs[0])
        } else {
            // Write relative path segment.
            self.psw.write(path_segs[1])
        }
    }
}

/// Writes all Primitives from a slice into a Write.
///
/// # Examples
/// 
/// ```
/// use svg_util::primitive::{Primitive, write_path};
///
/// let mut str = String::new();
/// let segs : [Primitive<i8>; 2] = [Primitive::Moveto((1,1)), Primitive::Lineto((2,2))];
/// write_path(&mut str, &segs, false, None).unwrap();
/// assert_eq!(str, "M1 1 2 2");
/// ```
pub fn write_path<'a, 'b, W, T>(sink: &mut W, primitives: &'b [Primitive<T>], pretty: bool, precision: Option<usize>) ->  Result<(), fmt::Error>
where W: 'a + fmt::Write, T: Copy + Add<T, Output=T> + Sub<T, Output=T> + Display + Zero {
    let mut pw = PathWriter::new(sink, pretty, precision);
    for primitive in primitives {
        try!(pw.write(primitive.clone()));
    }
    Ok(())
}