//! The geometric primitives used in SVG: moves, lines, bezier curves and arcs.
//!
//! blub

use path::{PathSeg, PathSegReader, PathSegToPrimitive, Error, PathSegWriter};
use std::fmt;
use std::fmt::Display;
use std::f32::{EPSILON, MIN_10_EXP};

/// A geometric primitive.
///
/// The geometric primitives that can represent all geometries supported by SVG.
/// Those are essentially absolute version of `PathSeg`s, without all the special cases like horizontal lines.
#[derive(PartialEq, Copy, Clone)]
pub enum Primitive {
    Closepath,
    Moveto((f32, f32)),
    Lineto((f32, f32)),
    CurvetoCubic((f32, f32), (f32, f32), (f32, f32)),
    CurvetoQuadratic((f32, f32), (f32, f32)),
    Arc(f32, f32, f32, bool, bool, (f32, f32)),
}

impl fmt::Debug for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let primitive = self.clone();
        let seg : PathSeg = primitive.into();
        seg.fmt(f)
    }
}

/// Parse a [`<line>`](http://www.w3.org/TR/SVG11/shapes.html#LineElement) into an array of primitives.
pub fn parse_line(x1: f32, y1: f32, x2: f32, y2: f32) -> [Primitive; 2] {
    [Primitive::Moveto((x1, y1)), Primitive::Lineto((x2, y2))]
}

/// Parse a [`<circle>`](http://www.w3.org/TR/SVG11/shapes.html#CircleElement) into an array of primitives.
///
/// Note that you need to check that r is positive.
pub fn parse_circle(cx: f32, cy: f32, r: f32) -> [Primitive; 4] {
    [Primitive::Moveto((cx+r, cy)),
     Primitive::Arc(r, r, 0.0, false, true, (cx-r, cy)),
     Primitive::Arc(r, r, 0.0, false, true, (cx+r, cy)),
     Primitive::Closepath]
}

/// Parse an [`<ellipse>`](http://www.w3.org/TR/SVG11/shapes.html#EllipseElement) into an array of primitives.
///
/// Note that you need to check that `rx` and `ry` are positive.
pub fn parse_ellipse(cx: f32, cy: f32, rx: f32, ry: f32) -> [Primitive; 4] {
    [Primitive::Moveto((cx+rx, cy)),
     Primitive::Arc(rx, ry, 0.0, false, true, (cx-rx, cy)),
     Primitive::Arc(rx, ry, 0.0, false, true, (cx+rx, cy)),
     Primitive::Closepath]
}

/// Parse a [`<rect>`](http://www.w3.org/TR/SVG11/shapes.html#RectElement) without corners into an array of primitives.
///
/// Note that `width` and `height` need to be larger than zero according to the spec.
pub fn parse_simple_rect(x: f32, y: f32, width: f32, height: f32) -> [Primitive; 6]{
    [Primitive::Moveto((x, y)),
     Primitive::Lineto((x+width, y)),
     Primitive::Lineto((x+width, y+height)),
     Primitive::Lineto((x, y+height)),
     Primitive::Lineto((x, y)),
     Primitive::Closepath]
}

/// Parses `Primitive`s from a path string.
pub struct PathReader<'a> {
    path_seg_reader: PathSegReader<'a>,
    path_seg_to_primitive: PathSegToPrimitive,
}

impl<'a> PathReader<'a> {
    /// Creates a new `PathReader` using for instance a `&str` or a `&[u8]`.
    pub fn new<B: AsRef<[u8]> + ?Sized>(src: &'a B) -> PathReader<'a> {
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
    pub fn pop_one_pathseg(&mut self) -> Option<Result<Primitive, Error>> {
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
    pub fn precision(&self) -> u8 {
        self.path_seg_reader.precision()
    }
}

/// Parse all `Primitive`s from a [`<path>`](http://www.w3.org/TR/SVG11/paths.html#PathElement) and put them in a newly allocated array.
///
/// Returns all path segments as `Primitive`s that were parsed until an error occurred or the string was empty,
/// the error if one occured and the maximum precision.
pub fn parse_path<'a, B: ?Sized>(src: &'a B) -> (Vec<Primitive>, Option<Error>, u8)
where B: AsRef<[u8]> {
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
pub struct PathPrimitives<'a> {
    reader: PathReader<'a>,
    done: bool,
}

impl<'a> Iterator for PathPrimitives<'a> {
    type Item = Result<Primitive, Error>;

    fn next(&mut self) -> Option<Result<Primitive, Error>> {
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

impl<'a> From<PathReader<'a>> for PathPrimitives<'a> {
    fn from(reader: PathReader<'a>) -> PathPrimitives<'a> {
        PathPrimitives {
            reader: reader,
            done: false,
        }
    }
}

impl<'a> IntoIterator for PathReader<'a> {
    type Item = Result<Primitive, Error>;
    type IntoIter = PathPrimitives<'a>;

    fn into_iter(self) -> PathPrimitives<'a> {
        self.into()
    }
}

pub struct PathWriter<'a, W: 'a + fmt::Write> {
    psw: PathSegWriter<'a, W>,
    /// The position we're currently at.
    pos: (f32, f32),
    /// Where we moved to with the last move.
    last_move: (f32, f32),
    /// The value under which we consider values to be zero.
    epsilon: f32,
}

impl <'a, W: 'a + fmt::Write> PathWriter<'a, W> {
    pub fn new(sink: &'a mut W, pretty: bool, precision: Option<u8>) -> PathWriter<'a, W> {
        let epsilon = match precision {
            Some(p) => {
                let exponent : i32 = -1 * i32::from(p);
                if exponent >= MIN_10_EXP {
                    // Example: precision 1
                    // 0.06 > 0.05 (because it will get rounded to 0.1) 
                    // 0.04 < 0.05 (because it will get rounded to 0.0)
                    10.0f32.powi(exponent) / 2.0
                } else {
                    EPSILON
                }
            }
            None => EPSILON,
        };
        PathWriter {
            psw: PathSegWriter::new(sink, pretty, precision),
            pos: (0.0, 0.0),
            last_move: (0.0, 0.0),
            epsilon: epsilon,
        }
    }
}

fn to_rel(pos: (f32, f32), abs: (f32, f32)) -> (f32, f32) {
    let (abs_x, abs_y) = abs;
    let (pos_x, pos_y) = pos;
    (abs_x - pos_x, abs_y - pos_y)
}

impl <'a, W: 'a + fmt::Write> PathWriter<'a, W> {
    pub fn write(&mut self, primitive: Primitive) -> Result<(), fmt::Error> {
        let path_segs = match primitive {
            Primitive::Closepath => {
                // Current position moves back on closepath.
                self.pos = self.last_move;
                let pathseg : PathSeg = PathSeg::Closepath;
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
                let (pos_x, pos_y) = self.pos;
                let (x, y) = p;
                let path_segs = if (x - pos_x).abs() <= self.epsilon {
                    [ PathSeg::LinetoVerticalAbs(y), PathSeg::LinetoVerticalRel(y - pos_y) ]
                } else if (y - pos_y).abs() <= self.epsilon {
                    [ PathSeg::LinetoHorizontalAbs(x), PathSeg::LinetoHorizontalRel(x - pos_x) ]
                } else {
                    [ PathSeg::LinetoAbs(p), PathSeg::LinetoRel(to_rel(self.pos, p)) ]
                };
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
/// let segs = [Primitive::Moveto((1.0,1.0)), Primitive::Lineto((2.0,2.0))];
/// write_path(&mut str, &segs, false, None).unwrap();
/// assert_eq!(str, "M1 1 2 2");
/// ```
pub fn write_path<'a, 'b, W>(sink: &mut W, primitives: &'b [Primitive], pretty: bool, precision: Option<u8>) ->  Result<(), fmt::Error>
where W: 'a + fmt::Write {
    let mut pw = PathWriter::new(sink, pretty, precision);
    for primitive in primitives {
        try!(pw.write(primitive.clone()));
    }
    Ok(())
}