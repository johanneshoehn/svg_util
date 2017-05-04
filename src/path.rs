//! Parsing and printing path data.
//!
//! Path data is used in the `d` attribute of `path` elements.
//! This module is about parsing and printing the segments, represented as
//! `PathSeg`.
//! `PathSeg`s map directly to all the different variants of segments from the
//! SVG specification.
//! If you want to do rendering or transformations working on `Primitive`s 
//! from the `primitive` module is probably a better idea.

use primitive::Primitive;
use util::{TokenWritten, DropLeadingZero, Count};

use std::error;
use std::fmt;
use std::fmt::Write;
use std::iter::Iterator;
use std::str::from_utf8_unchecked;
use std::str::FromStr;
use std::convert::From;

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
#[repr(u8)]
/// The different types of `PathSeg`s.
///
/// The `u8` representation using the letters should reduce the amount of conversion that needs to
/// be done.
/// But this actually needs to get tested.
enum PathSegType {
    ClosepathAbs = b'Z',
    ClosepathRel = b'z',
    MovetoAbs = b'M',
    MovetoRel = b'm',
    LinetoAbs = b'L',
    LinetoRel = b'l',
    CurvetoCubicAbs = b'C',
    CurvetoCubicRel = b'c',
    CurvetoQuadraticAbs = b'Q',
    CurvetoQuadraticRel = b'q',
    ArcAbs = b'A',
    ArcRel = b'a',
    LinetoHorizontalAbs = b'H',
    LinetoHorizontalRel = b'h',
    LinetoVerticalAbs = b'V',
    LinetoVerticalRel = b'v',
    CurvetoCubicSmoothAbs = b'S',
    CurvetoCubicSmoothRel = b's',
    CurvetoQuadraticSmoothAbs = b'T',
    CurvetoQuadraticSmoothRel = b't',
}

impl PathSegType {
    fn from_u8(c: u8) -> Option<PathSegType> {
        match c {
            b'Z' => Some(PathSegType::ClosepathAbs),
            b'z' => Some(PathSegType::ClosepathRel),
            b'M' => Some(PathSegType::MovetoAbs),
            b'm' => Some(PathSegType::MovetoRel),
            b'L' => Some(PathSegType::LinetoAbs),
            b'l' => Some(PathSegType::LinetoRel),
            b'C' => Some(PathSegType::CurvetoCubicAbs),
            b'c' => Some(PathSegType::CurvetoCubicRel),
            b'Q' => Some(PathSegType::CurvetoQuadraticAbs),
            b'q' => Some(PathSegType::CurvetoQuadraticRel),
            b'A' => Some(PathSegType::ArcAbs),
            b'a' => Some(PathSegType::ArcRel),
            b'H' => Some(PathSegType::LinetoHorizontalAbs),
            b'h' => Some(PathSegType::LinetoHorizontalRel),
            b'V' => Some(PathSegType::LinetoVerticalAbs),
            b'v' => Some(PathSegType::LinetoVerticalRel),
            b'S' => Some(PathSegType::CurvetoCubicSmoothAbs),
            b's' => Some(PathSegType::CurvetoCubicSmoothRel),
            b'T' => Some(PathSegType::CurvetoQuadraticSmoothAbs),
            b't' => Some(PathSegType::CurvetoQuadraticSmoothRel),
            _ => None,
        }
    }
}

impl From<PathSegType> for char {
    fn from(pst: PathSegType) -> char {
        pst as u8 as char
    }
}

#[test]
// When converting a char to the `PathSegType` and converting it back it should still be the same
// char.
fn path_seg_type_test() {
    for i in 0..257 {
        let c0 = i as u8 as char;
        match i as u8 {
            b'A'...b'Z' | b'a'...b'z' => {
                if let Some(path_seg_type) = PathSegType::from_u8(i as u8) {
                    let c1 = path_seg_type.into();
                    assert_eq!(c0, c1);
                }
            }
            // All non letters shouldn't convert to a `PathSegType`
            non_letter => {
                assert_eq!(PathSegType::from_u8(non_letter), None);
            }
        }
    }
}

/// One segment of a path.
///
/// Note that repititions without a new character to define the type of a path segment get treated
/// as multiple `PathSeg`s.
/// E.g.: `l 1,1 1,1` and `l 1,1 l 1,1` are both parsed as two `PathSeg`s.
///
/// The different types of segments are explained in detail in the [SVG Specification](http://www.w3.org/TR/SVG11/paths.html#PathData)
#[derive(PartialEq, Copy, Clone)]
pub enum PathSeg {
    /// Closes the path and moves back to where the last moveto started the subpath.
    Closepath,
    /// Starts a new subpath at an absolute position.
    MovetoAbs((f32, f32)),
    /// Starts a new subpath at a position relative to the last current position.
    MovetoRel((f32, f32)), 
    /// Draws a line from the current position to an absolute position.
    LinetoAbs((f32, f32)),
    /// Draws a line from the current position to a position relative to the current position.
    LinetoRel((f32, f32)),
    /// Draws a cubic Bézier curve with absolute positions for first control point, second control point and end position.
    CurvetoCubicAbs((f32, f32), (f32, f32), (f32, f32)),
    /// Draws a cubic Bézier curve with relative positions for first control point, second control point and end position.
    CurvetoCubicRel((f32, f32), (f32, f32), (f32, f32)),
    /// Draws a quadratic Bézier curve with absolute positions for the control point and the end position.
    CurvetoQuadraticAbs((f32, f32), (f32, f32)),
    /// Draws a quadratic Bézier curve with positions relative to the start position.
    CurvetoQuadraticRel((f32, f32), (f32, f32)),
    /// Draws an arc with radius in x direction, radius in y direction, rotation of the axis, large arc flag, sweep flag and the absolute end position.
    ArcAbs(f32, f32, f32, bool, bool, (f32, f32)),
    /// Draws an arc with radius in x direction, radius in y direction, rotation of the axis, large arc flag, sweep flag and the end position relative current to the current position.
    ArcRel(f32, f32, f32, bool, bool, (f32, f32)),
    /// Draws a horizontal line to an absolute x position.
    LinetoHorizontalAbs(f32),
    /// Draws a horizontal line with a certain length.
    LinetoHorizontalRel(f32),
    /// Draws a vertical line to an absolute y position.
    LinetoVerticalAbs(f32),
    /// Draws a vertical line with a certain length.
    LinetoVerticalRel(f32),
    /// Draws a cubic Bézier curve with a calculated first control point and absolute positions for the second control point and end position.
    CurvetoCubicSmoothAbs((f32, f32), (f32, f32)),
    /// Draws a cubic Bézier curve with a calculated first control point and relative positions for the second control point and end position.
    CurvetoCubicSmoothRel((f32, f32), (f32, f32)),
    /// Draws a quadratic Bézier curve with a calculated control point and the absolute end position.
    CurvetoQuadraticSmoothAbs((f32, f32)),
    /// Draws a quadratic Bézier curve with a calculated control point and the relative end position.
    CurvetoQuadraticSmoothRel((f32, f32)),
}

impl From<Primitive> for PathSeg {
    fn from(primitive: Primitive) -> PathSeg {
        match primitive {
            Primitive::Closepath => PathSeg::Closepath,
            Primitive::Moveto(p) => PathSeg::MovetoAbs(p),
            Primitive::Lineto(p) => PathSeg::LinetoAbs(p),
            Primitive::CurvetoCubic(p1, p2, p) => PathSeg::CurvetoCubicAbs(p1, p2, p),
            Primitive::CurvetoQuadratic(p1, p) => PathSeg::CurvetoQuadraticAbs(p1, p),
            Primitive::Arc(r1, r2, rotation, large_arc_flag, sweep_flag, p) =>
                PathSeg::ArcAbs(r1, r2, rotation, large_arc_flag, sweep_flag, p)
        }
    }
}

impl PathSeg {
    fn path_seg_type(self) -> PathSegType {
        match self {
            PathSeg::Closepath => PathSegType::ClosepathRel,
            PathSeg::MovetoAbs( .. ) => PathSegType::MovetoAbs,
            PathSeg::MovetoRel( .. ) => PathSegType::MovetoRel,
            PathSeg::LinetoAbs( .. ) => PathSegType::LinetoAbs,
            PathSeg::LinetoRel( .. ) => PathSegType::LinetoRel,
            PathSeg::CurvetoCubicAbs( .. ) => PathSegType::CurvetoCubicAbs,
            PathSeg::CurvetoCubicRel( .. ) => PathSegType::CurvetoCubicRel,
            PathSeg::CurvetoQuadraticAbs( .. ) => PathSegType::CurvetoQuadraticAbs,
            PathSeg::CurvetoQuadraticRel( .. ) => PathSegType::CurvetoQuadraticRel,
            PathSeg::ArcAbs( .. ) => PathSegType::ArcAbs,
            PathSeg::ArcRel( .. ) => PathSegType::ArcRel,
            PathSeg::LinetoHorizontalAbs( .. ) => PathSegType::LinetoHorizontalAbs,
            PathSeg::LinetoHorizontalRel( .. ) => PathSegType::LinetoHorizontalRel,
            PathSeg::LinetoVerticalAbs( .. ) => PathSegType::LinetoVerticalAbs,
            PathSeg::LinetoVerticalRel( .. ) => PathSegType::LinetoVerticalRel,
            PathSeg::CurvetoCubicSmoothAbs( .. ) => PathSegType::CurvetoCubicSmoothAbs,
            PathSeg::CurvetoCubicSmoothRel( .. ) => PathSegType::CurvetoCubicSmoothRel,
            PathSeg::CurvetoQuadraticSmoothAbs( .. ) => PathSegType::CurvetoQuadraticSmoothAbs,
            PathSeg::CurvetoQuadraticSmoothRel( .. ) => PathSegType::CurvetoQuadraticSmoothRel,
        }
    }
}

impl fmt::Debug for PathSeg {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let mut writer = PathSegWriter::new(f, true);
        let seg = self.clone();
        writer.write(seg)
    }
}

/// Parses one `PathSeg` at a time.
///
/// `&mut PathSegReader` implements an `Iterator` that parses one `PathSeg` each time `next()` is called.
/// Using the `Iterator` allows parsing and further processing without needing to allocate memory
/// on the heap.
///
/// # Failures
///
/// 
/// The SVG standard mandates that everything up to an error is processed, but it still would be
/// nice to have some warnings.
///
/// # Examples
///
/// ```
/// use svg_util::path::PathSegReader;
///
/// let mut parser = PathSegReader::new("M 0 0 h 1 v 1 h -1 z");
/// 
/// for token in parser {
///     match token {
///         Ok(segment) => println!("{:?}", segment),
///         Err(error) => {}
///     }
/// }
/// ```
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub struct PathSegReader<'a> {
    /// The rest of the string to be parsed.
    src: &'a [u8],
    /// The type of the last command that was parsed.
    /// This is needed because when the Type doesn't change it doesn't need to be repeated.
    /// E.g. `V 0 1` is equal to `V 0 V 1`
    mode: Option<PathSegType>,
    /// The first PathSeg must be a move. If this bool is set a non-move will lead to an error.
    first: bool,
    /// The maximum precision that occured up to now.
    max_precision: u8,
}

// TODO: Add more info like where in the string the error happened.
// (begin of `PathSeg` + begin of the Error)
/// Errors that can occur while parsing.
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum Error {
    /// Nothing left in the string.
    /// This can occur when using `from_str`,
    EndOfString,
    /// Expected the mode to be set, but got another character.
    ExpectedModeCharacter,
    /// Paths need to start with a Moveto command, but this one didn't.
    ExpectedMove,
    /// Expected a number but got something else.
    ExpectedNumber,
    /// Expected a flag, `0` or `1`.
    ExpectedFlag
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::EndOfString => write!(f, "Path string ended, but expected further numbers"),
            Error::ExpectedModeCharacter => write!(f, "Expected a new command character"),
            Error::ExpectedMove => write!(f, "Expected a `M` or `m`"),
            Error::ExpectedNumber => write!(f, "Expected a number"),
            Error::ExpectedFlag => write!(f, "Expected a flag (`0` or `1`)")
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
                match *self {
            Error::EndOfString => "Path string ended, but expected further numbers",
            Error::ExpectedModeCharacter => "Expected a new command character",
            Error::ExpectedMove => "Expected a `M` or `m`",
            Error::ExpectedNumber => "Expected a number",
            Error::ExpectedFlag => "Expected a flag (`0` or `1`)"
        }
    }
}

// FIXME: maybe an own error type makes more sense?
impl FromStr for PathSeg {
    type Err = Error;
    fn from_str(s: &str) -> Result<PathSeg, Error> {
        let mut reader = PathSegReader::new_arbitrary(s);
        match reader.pop_one_pathseg() {
            Some(result) => result,
            None => Err(Error::EndOfString)
        }
        // FIXME: the rest of the string needs to be empty!
    }
}

impl<'a> PathSegReader<'a> {
    /// Creates a new `PathSegReader` using for instance a `&str` or a `&[u8]`.
    pub fn new<B: AsRef<[u8]> + ?Sized>(src: &'a B) -> PathSegReader<'a> {
        PathSegReader {
            src: src.as_ref(),
            mode: None,
            first: true,
            max_precision: 0,
        }
    }
    // In contrast to the reader created by `new` this allows parsing arbitrary
    // `PathSeg` sequences, that don't have to start with move `PathSeg`s.
    fn new_arbitrary(src: &'a str) -> PathSegReader<'a> {
        PathSegReader {
            src: src.as_ref(),
            mode: None,
            first: false,
            max_precision: 0,
        }
    }

    /// Parses the first `PathSeg` and removes it from the source.
    ///
    /// Returns `None` when the source doesn't contain any `PathSeg`s
    /// anymore (empty or only whitespace).
    /// Otherwise it returns either the `PathSeg` or an `Error` if one
    /// occured.
    pub fn pop_one_pathseg(&mut self) -> Option<Result<PathSeg, Error>> {
        self.remove_leading_whitespace();
        if self.src.is_empty() {
            None
        } else {
            Some(self.pop_empty())
        }
    }

    /// Returns the maximum precision encountered so far.
    pub fn precision(&self) -> u8 {
        self.max_precision
    }

    // Pop one `PathSeg` with the precondition that there are
    // `PathSegs` left.
    fn pop_empty(&mut self) -> Result<PathSeg, Error> {

        // Look ahead if the next char changes the type of the `PathSeg`
        let mode = if let Some(mode) = PathSegType::from_u8(self.src[0]) {
            self.mode = Some(mode);
            self.src = &self.src[1..];
            self.remove_leading_whitespace();
            mode
        } else {
            // Continuation of previous command  E.g.: `h 1, 1` is equal to `h 1 h 1`
            self.remove_leading_comma_whitespace();
            self.mode.ok_or(Error::ExpectedModeCharacter)?
        };

        // First Path Segment needs to be a Moveto
        if self.first {
            if mode != PathSegType::MovetoAbs || mode != PathSegType::MovetoRel {
                return Err(Error::ExpectedMove);
            }
            self.first = false;
        }

        match mode {
            PathSegType::ClosepathAbs | PathSegType::ClosepathRel => {
                // `Closepath` can't be repeated without reusing 'z'/'Z'.
                self.mode = None;
                Ok(PathSeg::Closepath)
            }
            PathSegType::MovetoAbs => {
                Ok(PathSeg::MovetoAbs(self.get_coordinate_pair()?))
            }
            PathSegType::MovetoRel => {
                Ok(PathSeg::MovetoRel(self.get_coordinate_pair()?))
            }
            PathSegType::LinetoAbs => {
                Ok(PathSeg::LinetoAbs(self.get_coordinate_pair()?))
            }
            PathSegType::LinetoRel => {
                Ok(PathSeg::LinetoRel(self.get_coordinate_pair()?))
            }
            PathSegType::CurvetoCubicAbs => {
                let (p1, p2, p) = self.get_three_coordinate_pairs()?;
                Ok(PathSeg::CurvetoCubicAbs(p1, p2, p))
            }
            PathSegType::CurvetoCubicRel => {
                let (p1, p2, p) = self.get_three_coordinate_pairs()?;
                Ok(PathSeg::CurvetoCubicRel(p1, p2, p))
            }
            PathSegType::CurvetoQuadraticAbs => {
                let (p1, p) = self.get_two_coordinate_pairs()?;
                Ok(PathSeg::CurvetoQuadraticAbs(p1, p))
            }
            PathSegType::CurvetoQuadraticRel => {
                let (p1, p) = self.get_two_coordinate_pairs()?;
                Ok(PathSeg::CurvetoQuadraticRel(p1, p))
            }
            PathSegType::ArcAbs => {
                let (r1, r2, rotation, large_arc_flag, sweep_flag, p) =
                    self.get_arc_argument()?;
                Ok(PathSeg::ArcAbs(r1, r2, rotation, large_arc_flag, sweep_flag, p))
            }
            PathSegType::ArcRel => {
                let (r1, r2, rotation, large_arc_flag, sweep_flag, p) =
                    self.get_arc_argument()?;
                Ok(PathSeg::ArcRel(r1, r2, rotation, large_arc_flag, sweep_flag, p))
            }
            PathSegType::LinetoHorizontalAbs => {
                Ok(PathSeg::LinetoHorizontalAbs(self.get_number(false)?))
            }
            PathSegType::LinetoHorizontalRel => {
                Ok(PathSeg::LinetoHorizontalRel(self.get_number(false)?))
            }
            PathSegType::LinetoVerticalAbs => {
                Ok(PathSeg::LinetoVerticalAbs(self.get_number(false)?))
            }
            PathSegType::LinetoVerticalRel => {
                Ok(PathSeg::LinetoVerticalRel(self.get_number(false)?))
            }
            PathSegType::CurvetoCubicSmoothAbs => {
                let (p2, p) = self.get_two_coordinate_pairs()?;
                Ok((PathSeg::CurvetoCubicSmoothAbs(p2, p)))
            }
            PathSegType::CurvetoCubicSmoothRel => {
                let (p2, p) = self.get_two_coordinate_pairs()?;
                Ok((PathSeg::CurvetoCubicSmoothRel(p2, p)))
            }
            PathSegType::CurvetoQuadraticSmoothAbs => {
                Ok(PathSeg::CurvetoQuadraticSmoothAbs(self.get_coordinate_pair()?))
            }
            PathSegType::CurvetoQuadraticSmoothRel => {
                Ok(PathSeg::CurvetoQuadraticSmoothRel(self.get_coordinate_pair()?))
            }
        }
    }

    fn remove_leading_whitespace(&mut self) {
        while let Some(c) = self.src.first() {
            if [0x20, 0x9, 0xD, 0xA].contains(c) {
                self.src = &self.src[1..];
            } else {
                return;
            }
        }
    }

    fn remove_leading_comma_whitespace(&mut self) {
        self.remove_leading_whitespace();
        if let Some(&b',') = self.src.first() {
            self.src = &self.src[1..];
            self.remove_leading_whitespace();
        }
    }

    fn get_number(&mut self, nonnegative: bool) -> Result<f32, Error> {
        let mut length = 0;
        let mut precision : u8 = 0;

        if !nonnegative {
            match self.src.get(length) {
                Some(&b'+') | Some(&b'-') => {
                    length += 1;
                }
                _ => {}
            }
        }

        while let Some(c) = self.src.get(length) {
            match *c {
                b'0'...b'9' => {
                    length += 1;
                }
                _ => break,
            }
        }
        if let Some(&b'.') = self.src.get(length) {
            length += 1;
            while let Some(c) = self.src.get(length) {
                match *c {
                    b'0'...b'9' => {
                        length += 1;
                        precision += 1;
                    }
                    _ => break,
                }
            }
            // "." is not a valid svg number.
            if length < 2 {
                return Err(Error::ExpectedNumber);
            }
        } else if length == 0 {
            // An empty string is not a valid svg number.
            return Err(Error::ExpectedNumber);
        }

        match self.src.get(length) {
            Some(&b'e') | Some(&b'E') => {
                length += 1;
                let negative = match self.src.get(length) {
                    Some(&b'+')  => {
                        length += 1;
                        false
                    }
                    Some(&b'-') => {
                        length += 1;
                        true
                    }
                    _ => false
                };
                let mut length_after_e = length;
                while let Some(c) = self.src.get(length_after_e) {
                    match *c {
                        b'0'...b'9' => {
                            length_after_e += 1;
                        }
                        _ => break,
                    }
                }
                if length_after_e == length {
                    // Digits need to follow after an `e`/`E`.
                    return Err(Error::ExpectedNumber);
                }
                // Parse the number after `e` into a `u8` and subtract it from the precision.
                // E.g. 0.1e1 == 1 with precision 0.
                let num = &self.src[length .. length_after_e];
                let numstring = unsafe { from_utf8_unchecked(num) };
                match u8::from_str(numstring) {
                    Ok(num) => {
                        if negative {
                            precision = precision.saturating_add(num);
                        } else {
                            precision = precision.saturating_sub(num);
                        }
                    }
                    _ => {}
                }
                length = length_after_e;
            }
            _ => {}
        }

        if precision > self.max_precision {
            self.max_precision = precision;
        }

        let (num, rest) = self.src.split_at(length);
        self.src = rest;

        // Safe because we just matched only numbers, '+', '-', '.', 'e' and 'E's.
        let numstring = unsafe { from_utf8_unchecked(num) };
        Ok(f32::from_str(numstring).unwrap())
    }

    fn get_coordinate_pair(&mut self) -> Result<(f32, f32), Error> {
        let x = self.get_number(false)?;
        self.remove_leading_comma_whitespace();
        let y = self.get_number(false)?;
        Ok((x, y))
    }

    fn get_two_coordinate_pairs(&mut self) -> Result<((f32, f32), (f32, f32)), Error> {
        let p1 = self.get_coordinate_pair()?;
        self.remove_leading_comma_whitespace();
        let p2 = self.get_coordinate_pair()?;
        Ok((p1, p2))
    }

    fn get_three_coordinate_pairs(&mut self) -> Result<((f32, f32), (f32, f32), (f32, f32)), Error> {
        let p1 = self.get_coordinate_pair()?;
        self.remove_leading_comma_whitespace();
        let p2 = self.get_coordinate_pair()?;
        self.remove_leading_comma_whitespace();
        let p3 = self.get_coordinate_pair()?;
        Ok((p1, p2, p3))
    }

    fn get_arc_argument(&mut self) -> Result<(f32, f32, f32, bool, bool, (f32, f32)), Error> {
        let (r1, r2) = self.get_coordinate_pair()?;
        self.remove_leading_comma_whitespace();
        let rotation = self.get_number(true)?;
        self.remove_leading_comma_whitespace();

        let large_arc_flag = match self.src.first() {
            Some(&b'1') => true,
            Some(&b'0') => false,
            _ => return Err(Error::ExpectedFlag),
        };
        self.src = &self.src[1..];

        self.remove_leading_comma_whitespace();

        let sweep_flag = match self.src.first() {
            Some(&b'1') => true,
            Some(&b'0') => false,
            _ => return Err(Error::ExpectedFlag),
        };
        self.src = &self.src[1..];

        self.remove_leading_comma_whitespace();

        let p = self.get_coordinate_pair()?;
        Ok((r1, r2, rotation, large_arc_flag, sweep_flag, p))
    }
}

/// Parse all `PathSeg`s and put them in a newly allocated array.
///
/// Returns all `PathSeg`s that were parsed until an error occurred or the string was empty,
/// the error if one occured and the maximum precision.
pub fn parse_all_pathsegs<'a, T, B: ?Sized>(src: &'a B) -> (Vec<PathSeg>, Option<Error>, u8)
where T: Copy + FromStr, B: AsRef<[u8]> {
    let mut parser = PathSegReader::new(src);
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

/// The `Iterator` for `PathSegReader`.
pub struct Segs<'a> {
    reader: PathSegReader<'a>,
    done: bool,
}

impl<'a> Iterator for Segs<'a> {
    type Item = Result<PathSeg, Error>;

    fn next(&mut self) -> Option<Result<PathSeg, Error>> {
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

impl<'a> From<PathSegReader<'a>> for Segs<'a> {
    fn from(reader: PathSegReader<'a>) -> Segs<'a> {
        Segs {
            reader: reader,
            done: false,
        }
    }
}

impl<'a> IntoIterator for PathSegReader<'a> {
    type Item = Result<PathSeg, Error>;
    type IntoIter = Segs<'a>;

    fn into_iter(self) -> Segs<'a> {
        self.into()
    }
}

/// Writes `PathSeg`s as strings either in a space-saving or human readable format.
///
/// Because this needs a mutable `Write` you first have to
/// let the `PathSegWriter` let go out of scope again before
/// using the `Write` again.
/// FIXME: example
pub struct PathSegWriter<'a, W: 'a + Write> {
    /// Where to write to.
    sink: &'a mut W,
    /// The type of the last written `PathSeg`.
    mode: Option<PathSegType>,
    /// Wheter to pretty-print or have an optimized output.
    pretty: bool,
    /// What kind the token was that was last written.
    last_token: TokenWritten,
}

impl<'a, W: 'a + Write> PathSegWriter<'a, W> {
    /// Creates a new `PathSegWriter`.
    ///
    /// The `pretty` argument decides wether to write in a space-saving or pretty, human-readable way.
    pub fn new(sink: &'a mut W, pretty: bool) -> PathSegWriter<'a, W> {
        PathSegWriter {
            sink: sink,
            mode: None,
            pretty: pretty,
            last_token: TokenWritten::NotANumber,
        }
    }
    
    /// Write one number
    fn write_num(&mut self, num: f32) -> Result<(), fmt::Error> {
        if self.pretty {
            // Always write a space when pretty printing.
            self.sink.write_char(' ')?;
            write!(self.sink, "{}", num)?;
        } else {
            let mut optimized_writer = DropLeadingZero::new(&mut self.sink, self.last_token);
            write!(optimized_writer, "{}", num)?;
            self.last_token = optimized_writer.finish_and_return_token_written()?;
        }
        Ok(())
    }
    
    /// Write a x, y pair of numbers.
    fn write_pair(&mut self, pair: (f32, f32)) -> Result<(), fmt::Error> {
        let (x,y) = pair;
        self.write_num(x)?;
        self.write_num(y)?;
        Ok(())
    }
    
    /// Write a flag (used in arcs).
    fn write_flag(&mut self, flag: bool) -> Result<(), fmt::Error> {
        if self.pretty || self.last_token != TokenWritten::NotANumber {
            self.sink.write_char(' ')?;
        }
        self.sink.write_char(if flag { '1' } else { '0' })?;
        self.last_token = TokenWritten::NotANumber;
        Ok(())
    }

    /// Write a `PathSeg`.
    pub fn write(&mut self, path_seg: PathSeg) -> Result<(), fmt::Error> {
        let old_mode = self.mode;
        let path_seg_type = path_seg.path_seg_type();
        self.mode = Some(path_seg_type);

        if self.pretty {
            match path_seg_type {
                // Moves should stand on new lines when pretty printing.
                PathSegType::MovetoAbs | PathSegType::MovetoRel => {
                    self.sink.write_char('\n')?;
                }
                // Other `PathSeg`s should be seperated by spaces.
                _ => {
                    self.sink.write_char(' ')?;
                }
            }
        }

        // Don't repeat the character for setting the type of the `PathSeg` when it's still the same.
        // Except: `z`/`Z` or when pretty printing.
        let need_mode_character = self.pretty || match (path_seg_type, old_mode) {
            (_, None) => true,
            (PathSegType::ClosepathAbs, _) => true,
            (PathSegType::ClosepathRel, _) => true,
            (PathSegType::LinetoAbs, Some(PathSegType::MovetoAbs)) => false,
            (PathSegType::LinetoRel, Some(PathSegType::MovetoRel)) => false,
            (new, Some(old)) => new != old           
        };

        if need_mode_character {
            self.sink.write_char(path_seg_type.into())?;
            self.last_token = TokenWritten::NotANumber;
        }

        match path_seg {
            PathSeg::Closepath => Ok(()),
            PathSeg::MovetoAbs(p) |
            PathSeg::MovetoRel(p) |
            PathSeg::LinetoAbs(p) |
            PathSeg::LinetoRel(p) =>
                self.write_pair(p),
            PathSeg::CurvetoCubicAbs(p1, p2, p) |
            PathSeg::CurvetoCubicRel(p1, p2, p) => {
                self.write_pair(p1)?;
                self.write_pair(p2)?;
                self.write_pair(p)
            }
            PathSeg::CurvetoQuadraticAbs(p1, p) |
            PathSeg::CurvetoQuadraticRel(p1, p) => {
                self.write_pair(p1)?;
                self.write_pair(p)
            }
            PathSeg::ArcAbs(r1, r2, rotation, large_arc_flag, sweep_flag, p) |
            PathSeg::ArcRel(r1, r2, rotation, large_arc_flag, sweep_flag, p) => {
                self.write_num(r1)?;
                self.write_num(r2)?;
                self.write_num(rotation)?;
                self.write_flag(large_arc_flag)?;
                self.write_flag(sweep_flag)?;
                self.write_pair(p)
            }
            PathSeg::LinetoHorizontalAbs(x) |
            PathSeg::LinetoHorizontalRel(x) =>
                self.write_num(x),
            PathSeg::LinetoVerticalAbs(y) |
            PathSeg::LinetoVerticalRel(y) =>
                self.write_num(y),
            PathSeg::CurvetoCubicSmoothAbs(p2, p) |
            PathSeg::CurvetoCubicSmoothRel(p2, p) => {
                self.write_pair(p2)?;
                self.write_pair(p)
            }
            PathSeg::CurvetoQuadraticSmoothAbs(p) |
            PathSeg::CurvetoQuadraticSmoothRel(p) =>
                self.write_pair(p),
        }
    }

    /// Test how much bytes writing a `PathSeg` would emit, without changing any state.
    pub fn test_write(&self, path_seg: PathSeg) -> usize {
        let mut count = Count { len: 0 };
        let mut psw_copy = PathSegWriter {
            sink: &mut count,
            mode: self.mode,
            pretty: self.pretty,
            last_token: self.last_token,
        };
        let res = psw_copy.write(path_seg);
        if res.is_err() {
            panic!("writing in a `Count` shouldn't return Errors");
        }
        psw_copy.sink.len
    }
}

/// Writes all `PathSeg`s from a slice into a `Write`.
///
/// # Examples
/// 
/// ```
/// use svg_util::path::{PathSeg, write_all_pathsegs};
///
/// let mut str = String::new();
/// let segs  = [PathSeg::MovetoAbs((1.0,1.0)), PathSeg::LinetoRel((1.0,1.0))];
/// write_all_pathsegs(&mut str, &segs, false).unwrap();
/// assert_eq!(str, "M1 1l1 1");
/// ```
pub fn write_all_pathsegs<'a, W: Write>(sink: &mut W, pathsegs: &'a[PathSeg], pretty: bool) -> Result<(), fmt::Error> {
    let mut writer = PathSegWriter::new(sink, pretty);
    for seg in pathsegs {
        writer.write(seg.clone())?;
    }
    Ok(())
}

/// Converts `PathSeg`s to `Primitive`s.
pub struct PathSegToPrimitive {
    /// The position we're currently at.
    pub pos: (f32, f32),
    /// Where we moved to with the last move.
    pub last_move: (f32, f32),
    /// The position we're going to predict for a cubic smooth `PathSeg`.
    pub cubic_smooth: (f32, f32),
    /// The position we're going to predict for a quadratic smooth `PathSeg`.
    pub quadratic_smooth: (f32, f32),
}

impl PathSegToPrimitive {
    /// Create a new `PathSegToPrimitive` converter.
    /// It saves the state necessary to transform relative, smooth, and vertical/horizontal Segments into `Primitive`s
    pub fn new() -> Self {
        PathSegToPrimitive {
            pos: (0.0, 0.0),
            last_move: (0.0, 0.0),
            cubic_smooth: (0.0, 0.0),
            quadratic_smooth: (0.0, 0.0),
        }
    }
}

fn to_abs(pos: (f32, f32), rel: (f32, f32)) -> (f32, f32) {
    (pos.0 + rel.0, pos.1 + rel.1)
}

fn predict(new_pos: (f32, f32), point: (f32, f32)) -> (f32, f32) {
    let diff = (new_pos.0 - point.0, new_pos.1 - point.1);
    (new_pos.0 + diff.0, new_pos.1 + diff.1)
}

impl PathSegToPrimitive {
    /// Convert a `PathSeg` to a `Primitive`.
    pub fn convert(&mut self, seg: PathSeg) -> Primitive {
        match seg {
            PathSeg::Closepath => {
                self.pos = self.last_move;
                self.cubic_smooth = self.pos;
                self.quadratic_smooth = self.pos;
                Primitive::Closepath
            }
            PathSeg::MovetoAbs(p) => {
                self.pos = p;
                self.cubic_smooth = p;
                self.quadratic_smooth = p;
                self.last_move = p;
                Primitive::Moveto(p)
            }
            PathSeg::MovetoRel(p) => {
                let p_abs = to_abs(self.pos, p);
                
                self.pos = p_abs;
                self.cubic_smooth = p_abs;
                self.quadratic_smooth = p_abs;
                self.last_move = p_abs;
                Primitive::Moveto(p_abs)
            }
            PathSeg::LinetoAbs(p) => {
                self.pos = p;
                self.cubic_smooth = p;
                self.quadratic_smooth = p;
                Primitive::Lineto(p)
            }
            PathSeg::LinetoRel(p) => {
                let p_abs = to_abs(self.pos, p);
                
                self.pos = p_abs;
                self.cubic_smooth = p_abs;
                self.quadratic_smooth = p_abs;
                Primitive::Lineto(p_abs)
            }
            PathSeg::CurvetoCubicAbs(p1, p2, p) => {
                self.pos = p;
                self.cubic_smooth = predict(p, p2);
                self.quadratic_smooth = p;
                Primitive::CurvetoCubic(p1, p2, p)
            }
            PathSeg::CurvetoCubicRel(p1, p2, p) => {
                let p1_abs = to_abs(self.pos, p1);
                let p2_abs = to_abs(self.pos, p2);
                let p_abs = to_abs(self.pos, p);
                
                self.pos = p_abs;
                self.cubic_smooth = predict(p_abs, p2_abs);
                self.quadratic_smooth = p_abs;
                Primitive::CurvetoCubic(p1_abs, p2_abs, p_abs)
            }
            PathSeg::CurvetoQuadraticAbs(p1, p) => {
                self.pos = p;
                self.cubic_smooth = p;
                self.quadratic_smooth = predict(p, p1);
                Primitive::CurvetoQuadratic(p1, p)
            }
            PathSeg::CurvetoQuadraticRel(p1, p) => {
                let p1_abs = to_abs(self.pos, p1);
                let p_abs = to_abs(self.pos, p);
                
                self.pos = p_abs;
                self.cubic_smooth = p_abs;
                self.quadratic_smooth = predict(p_abs, p1_abs);
                Primitive::CurvetoQuadratic(p1_abs, p_abs)
            }
            PathSeg::ArcAbs(r1, r2, rotation, large_arc_flag, sweep_flag, p) => {
                self.pos = p;
                self.cubic_smooth = p;
                self.quadratic_smooth = p;
                Primitive::Arc(r1, r2, rotation, large_arc_flag, sweep_flag, p)
            }
            PathSeg::ArcRel(r1, r2, rotation, large_arc_flag, sweep_flag, p) => {
                let p_abs = to_abs(self.pos, p);
                
                self.pos = p_abs;
                self.cubic_smooth = p_abs;
                self.quadratic_smooth = p_abs;
                Primitive::Arc(r1, r2, rotation, large_arc_flag, sweep_flag, p_abs)
            }
            PathSeg::LinetoHorizontalAbs(x) => {
                 let (_, y) = self.pos;
                 let p = (x, y);
                 
                 self.pos = p;
                 self.cubic_smooth = p;
                 self.quadratic_smooth = p;
                 Primitive::Lineto(p)
            }
            PathSeg::LinetoHorizontalRel(x) => {
                 let (x_old, y) = self.pos;
                 let p = (x + x_old, y);
                 
                 self.pos = p;
                 self.cubic_smooth = p;
                 self.quadratic_smooth = p;
                 Primitive::Lineto(p)
            }
            PathSeg::LinetoVerticalAbs(y) => {
                 let (x, _) = self.pos;
                 let p = (x, y);
                 
                 self.pos = p;
                 self.cubic_smooth = p;
                 self.quadratic_smooth = p;
                 Primitive::Lineto(p)
            }
            PathSeg::LinetoVerticalRel(y) => {
                 let (x, y_old) = self.pos;
                 let p = (x, y + y_old);
                 
                 self.pos = p;
                 self.cubic_smooth = p;
                 self.quadratic_smooth = p;
                 Primitive::Lineto(p)
            }
            PathSeg::CurvetoCubicSmoothAbs(p2, p) => {
                let p1 = self.cubic_smooth;
                
                self.pos = p;
                self.cubic_smooth = predict(p, p2);
                self.quadratic_smooth = p;
                Primitive::CurvetoCubic(p1, p2, p)
            }
            PathSeg::CurvetoCubicSmoothRel(p2, p) => {
                let p1_abs = self.cubic_smooth;
                let p2_abs = to_abs(self.pos, p2);
                let p_abs = to_abs(self.pos, p);
                
                self.pos = p_abs;
                self.cubic_smooth = predict(p_abs, p2_abs);
                self.quadratic_smooth = p_abs;
                Primitive::CurvetoCubic(p1_abs, p2_abs, p_abs)
            }
            PathSeg::CurvetoQuadraticSmoothAbs(p) => {
                let p1 = self.quadratic_smooth;
                
                self.pos = p;
                self.cubic_smooth = p;
                self.quadratic_smooth = predict(p, p1);
                Primitive::CurvetoQuadratic(p1, p)
            }
            PathSeg::CurvetoQuadraticSmoothRel(p) => {
                let p1_abs = self.quadratic_smooth;
                let p_abs = to_abs(self.pos, p);
                
                self.pos = p_abs;
                self.cubic_smooth = p_abs;
                self.quadratic_smooth = predict(p_abs, p1_abs);
                Primitive::CurvetoQuadratic(p1_abs, p_abs)
            }
        }
    }
}