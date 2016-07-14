use std::fmt::{Write, Error};

/// How the `SvgNumber` was written.
///
/// To know wether we need whitespace before writing a new `SvgNumber` we need to know what we wrote before.
/// Between `.1` and `.1` we can drop the whitespace as `.1.1` will be parsed as two numbers.
// TODO: Maybe rename into when a space needs to be emitted?
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum TokenWritten {
    /// Token written is whitespace, a `PathSeg` identifier char or a flag of an arc.
    /// Never needs following whitespace.
    NotANumber,
    /// Token written is a Number, consisting only of `+`/`-` and digits.
    /// Needs following whitespace if next Number starts with a digit or a `.`, not when starting with `+` or `-`.
    AsInteger,
    /// Token written is a Number, that also contains `.` and/or `e`/`E`.
    /// Needs following whitespace if next Number starts with a digit, not when starting with `.`, `+` or `-`.
    WithDotOrE,
}

/// Round a `f32` to the nearest `f32` that corresponds to a decimal number with max `precision` fractional digits.
pub fn round_precision(val: f32, precision: u8) -> f32 {
    // Convert to 64 bit float, to avoid overflow.
    let val : f64 = f64::from(val);
    let factor : f64 = (10.0f64).powi(precision as i32);
    let result : f64 = (val * factor).round() / factor;
    result as f32
}

pub fn round_precision_pair(val: (f32, f32), precision: u8) -> (f32, f32) {
    let (x, y) = val;
    (round_precision(x, precision), round_precision(y, precision))
}

#[test]
fn test_rounding() {
    // Some test variables.
    // Exhaustive testing is probably possible and should be done.
    assert_eq!(round_precision(0.4, 0), 0.0);
    assert_eq!(round_precision(0.5, 0), 1.0);
    assert_eq!(round_precision(0.6, 0), 1.0);
    assert_eq!(round_precision(0.04, 1), 0.0);
    assert_eq!(round_precision(0.15, 1), 0.2);
    assert_eq!(round_precision(0.36, 1), 0.4);
    assert_eq!(round_precision(0.99, 1), 1.0);
}

/// Optimizing filter for the output of one SVG number.
/// 
/// The filter does the following:
///  - Drops the leading zero if it exists (e.g. `-0.1` to `-.1`).
///  - Preprints a space if needed (e.g. ' .1' after `1`, but `.1` after `.1`).
///  - Keep track if a `.`, `e` or `E` was printed, so that the next number might not need a space.
///
/// It is important to call `finish_and_return_token_written()` afterwards, otherwise the `0` case won't be handled properly.
/// Todo: Maybe split this into two structs, whith one containing the other? This could lead to cleaner code.
pub struct DropLeadingZero<'a, W: Write + 'a> {
    /// the underlying `Write`r.
    writer: &'a mut W,
    drop_state: DropState,
    last_token_written: TokenWritten,
    // wether wrote a '.' or a 'e'/'E'
    pub wrote_e_or_point: bool,
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
enum DropState {
    /// We might still encounter a leading zero.
    Start,
    /// We wrote a `+` or `-`, and might still encounter a leading zero.
    WrotePlusMinus,
    // State where we know that it was safe that we dropped a leading zero or we didn't drop a leading zero.
    Pass,
    /// We just dropped a leading zero and might push it on output again if nothing follows, otherwise the number `0` will be formatted as the empty string.
    JustDroppedZero
}

impl <'a, W: Write> DropLeadingZero<'a, W> {
    pub fn new(writer: &'a mut W, last_token_written: TokenWritten) -> DropLeadingZero<'a, W> {
        DropLeadingZero {
            writer: writer,
            drop_state: DropState::Start,
            last_token_written: last_token_written,
            wrote_e_or_point: false,
        }
    }
    pub fn finish_and_return_token_written(self) -> Result<TokenWritten, Error> {
        if self.drop_state == DropState::JustDroppedZero {
            if self.last_token_written != TokenWritten::NotANumber {
                try!(self.writer.write_char(' '));
            }
            try!(self.writer.write_char('0'));
            
        }
        if self.wrote_e_or_point {
            Ok(TokenWritten::WithDotOrE)
        } else {
            Ok(TokenWritten::AsInteger)
        }
    }
}

impl <'a, W: Write> Write for DropLeadingZero<'a, W> {
    fn write_str(&mut self, s: &str) -> Result<(), Error> {
        // Don't change anything on empty output
        if s.is_empty() {
            return Ok(());
        }
        
        // Just pass through
        if self.drop_state == DropState::Pass {
            if s.contains('.') || s.contains('e') || s.contains('E') {
                self.wrote_e_or_point = true;
            }
            return self.writer.write_str(s);
        }
        
        // Handle the more complex cases in `write_char`
        let mut iter = s.chars();
        let ch = iter.next().unwrap();
        let rest = iter.as_str();
        
        try!(self.write_char(ch));
        self.write_str(rest)
    }
    fn write_char(&mut self, c: char) -> Result<(), Error> {
        match self.drop_state {
            DropState::Pass => {
                if c == '.' || c == 'e' || c == 'E' {
                    self.wrote_e_or_point = true;
                }
                return self.writer.write_char(c);
            }
            DropState::JustDroppedZero => {
                if self.last_token_written == TokenWritten::AsInteger {
                    try!(self.writer.write_char(' '));
                }
                self.drop_state = DropState::Pass;
                if c == '.' || c == 'e' || c == 'E' {
                    self.wrote_e_or_point = true;
                } else if self.last_token_written == TokenWritten::WithDotOrE {
                    try!(self.writer.write_char(' '));
                }
                return self.writer.write_char(c);
            }
            DropState::WrotePlusMinus => {
                match c {
                    // Drop zero on encountering and signal it in the state.
                    '0' => {
                        // Dirty fix: otherwise this case will break on `"-0"` and will output `"- 0"` instead.
                        self.last_token_written = TokenWritten::NotANumber;
                        
                        self.drop_state = DropState::JustDroppedZero;
                        Ok(())
                    }
                    '.' | 'e' | 'E' => {
                        self.wrote_e_or_point = true;
                        self.drop_state = DropState::Pass;
                        self.writer.write_char(c)
                    }
                    _ => {
                        self.drop_state = DropState::Pass;
                        self.writer.write_char(c)
                    }
                }
            }
            DropState::Start => {
                match c {
                    // Drop zero on encountering and signal it in the state.
                    '+' | '-' => {
                        self.drop_state = DropState::WrotePlusMinus;
                        self.writer.write_char(c)
                    }
                    '0' => {
                        self.drop_state = DropState::JustDroppedZero;
                        Ok(())
                    }
                    '.' | 'e' | 'E' => {
                        self.wrote_e_or_point = true;
                        self.drop_state = DropState::Pass;
                        if self.last_token_written == TokenWritten::AsInteger {
                            try!(self.writer.write_char(' '));
                        }
                        self.writer.write_char(c)
                    }
                    _ => {
                        self.drop_state = DropState::Pass;
                        if self.last_token_written != TokenWritten::NotANumber {
                            try!(self.writer.write_char(' '));
                        }
                        self.writer.write_char(c)
                    }
                }
            }
        }
    }
}

// TODO: the code for `DropLeadingZero` is pretty hacky, so there should be more comprehensive testing.
#[test]
fn test_drop() {
    let mut s = String::new();
    let res = {let mut d = DropLeadingZero::new(&mut s, TokenWritten::NotANumber);
    let f : f64 = 0.1;
    write!(&mut d, "{}", f)};
    assert_eq!(s, ".1");
    assert!(res.is_ok());
    
    let mut s = String::new();
    {
        let mut d = DropLeadingZero::new(&mut s, TokenWritten::NotANumber);
        let f : f64 = 0.0;
        write!(&mut d, "{}", f).unwrap();
        d.finish_and_return_token_written().unwrap();
    }
    assert_eq!(s, "0");
    
    let mut s = String::new();
    let token = {
        let mut d = DropLeadingZero::new(&mut s, TokenWritten::NotANumber);
        let f : f64 = -0.0;
        write!(&mut d, "{}", f).unwrap();
        d.finish_and_return_token_written().unwrap()
    };
    assert_eq!(token, TokenWritten::AsInteger);
    assert_eq!(s, "0");
    
    let mut s = String::new();
    {
        let mut d = DropLeadingZero::new(&mut s, TokenWritten::NotANumber);
        write!(&mut d, "-0").unwrap();
        d.finish_and_return_token_written().unwrap();
    }
    assert_eq!(s, "-0");
    
    let mut s = String::new();
    let token = {
        let mut d = DropLeadingZero::new(&mut s, TokenWritten::AsInteger);
        let f : f64 = 0.1;
        write!(&mut d, "{}", f).unwrap();
        d.finish_and_return_token_written().unwrap()
    };
    assert_eq!(token, TokenWritten::WithDotOrE);
    assert_eq!(s, " .1");
}

/// A struct implementing `Write` that just counts the bytes written.
#[derive(Copy, Clone)]
pub struct Count{ pub len: usize}

impl Write for Count {
    fn write_str(&mut self, s: &str) -> Result<(), Error> {
        self.len += s.len();
        Ok(())
    }
}

#[test]
fn test_count() {
    let mut count = Count{len: 0};
    let result = count.write_str("hello world!");
    assert!(result.is_ok());
    assert_eq!(count.len, 12);
}