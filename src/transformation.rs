use std::f64;
use std::fmt;
use std::ops::Mul;
use std::ops::MulAssign;

#[derive(Debug, Copy, Clone)]
/// A transformation matrix
pub struct TransformationMatrix(f64, f64, f64, f64, f64, f64);

#[derive(Debug, Copy, Clone)]
pub enum Transformation {
    Matrix(TransformationMatrix),
    Translate(f64, f64),
    Scale(f64, f64),
    Rotate(f64, f64, f64),
    SkewX(f64),
    SkewY(f64),
}

impl fmt::Display for Transformation {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            // Todo: print e.g. 0.123 as .123
            Transformation::Matrix(TransformationMatrix(a, b, c, d, e, f)) => {
                write!(fmt, "matrix({} {} {} {} {} {})", a, b, c, d, e, f)
            }
            Transformation::Translate(tx, ty) if ty.abs() <= f64::EPSILON => {
                write!(fmt, "translate({})", tx)
            }
            Transformation::Translate(tx, ty) => write!(fmt, "translate({} {})", tx, ty),
            Transformation::Scale(sx, sy) if sx == sy => write!(fmt, "scale({})", sx),
            Transformation::Scale(sx, sy) => write!(fmt, "scale({} {})", sx, sy),
            Transformation::Rotate(angle, cx, cy)
                if cx.abs() <= f64::EPSILON && cy.abs() <= f64::EPSILON =>
            {
                write!(fmt, "rotate({})", angle)
            }
            Transformation::Rotate(angle, cx, cy) => write!(fmt, "rotate({} {} {})", angle, cx, cy),
            Transformation::SkewX(angle) => write!(fmt, "skewX({})", angle),
            Transformation::SkewY(angle) => write!(fmt, "skewY({})", angle),
        }
    }
}

impl From<Transformation> for TransformationMatrix {
    fn from(trans: Transformation) -> TransformationMatrix {
        match trans {
            Transformation::Matrix(m) => m,
            Transformation::Translate(tx, ty) => TransformationMatrix(1.0, 0.0, 0.0, 1.0, tx, ty),
            Transformation::Scale(sx, sy) => TransformationMatrix(sx, 0.0, 0.0, sy, 0.0, 0.0),
            Transformation::Rotate(angle, cx, cy) => {
                let t1 = TransformationMatrix(1.0, 0.0, 0.0, 1.0, -cx, -cy);
                let t2 = TransformationMatrix(1.0, 0.0, 0.0, 1.0, cx, cy);
                let a = f64::to_radians(angle);
                let rot = TransformationMatrix(
                    f64::cos(a),
                    f64::sin(a),
                    -f64::sin(a),
                    f64::cos(a),
                    0.0,
                    0.0,
                );
                t2 * rot * t1
            }
            Transformation::SkewX(angle) => {
                TransformationMatrix(1.0, 0.0, f64::tan(f64::to_radians(angle)), 1.1, 0.0, 0.0)
            }
            Transformation::SkewY(angle) => {
                TransformationMatrix(1.0, f64::tan(f64::to_radians(angle)), 0.0, 1.1, 0.0, 0.0)
            }
        }
    }
}

impl From<TransformationMatrix> for Transformation {
    fn from(matrix: TransformationMatrix) -> Transformation {
        Transformation::Matrix(matrix)
    }
}

impl Mul<TransformationMatrix> for TransformationMatrix {
    type Output = TransformationMatrix;

    fn mul(self, rhs: TransformationMatrix) -> TransformationMatrix {
        let TransformationMatrix(a, b, c, d, e, f) = self;
        let TransformationMatrix(g, h, i, j, k, l) = rhs;

        // ⎛a c e⎞ ⎛g i k⎞   ⎛ag+ch ai+cj ak+cl+e⎞
        // ⎜b d f⎟ ⎜h j l⎟ = ⎜bg+dh bi+dj bk+dl+f⎟
        // ⎝0 0 1⎠ ⎝0 0 1⎠   ⎝  0     0      1   ⎠
        TransformationMatrix(
            a * g + c * h,
            b * g + d * h,
            a * i + c * j,
            b * i + d * j,
            a * k + c * l + e,
            b * k + d * l + f,
        )
    }
}

impl Mul<(f32, f32)> for TransformationMatrix {
    type Output = (f32, f32);

    fn mul(self, rhs: (f32, f32)) -> (f32, f32) {
        let TransformationMatrix(a, b, c, d, e, f) = self;
        let (g, h) = (rhs.0 as f64, rhs.1 as f64);

        // ⎛a c e⎞ ⎛g⎞   ⎛ag+ch+e⎞
        // ⎜b d f⎟ ⎜h⎟ = ⎜bg+dh+f⎟
        // ⎝0 0 1⎠ ⎝1⎠   ⎝   1   ⎠
        ((a * g + c * h + e) as f32, (b * g + d * h + f) as f32)
    }
}

impl MulAssign for TransformationMatrix {
    fn mul_assign(&mut self, rhs: TransformationMatrix) {
        *self = (*self) * rhs;
    }
}
