package core.scm;

import core.procedures.math.*;
import core.procedures.math.trigonometry.Atan;
import core.procedures.math.trigonometry.Cos;
import core.procedures.math.trigonometry.Sin;
import core.utils.Utils;

import java.math.BigDecimal;

/**
 * TODO Implement rational Real and Imaginary parts: 1/2+3/4i
 */
public final class BigComplex extends Number {

  /* Imaginary unit (i) */
  public static final BigComplex I = new BigComplex(BigDecimal.ZERO, BigDecimal.ONE);

  private final BigDecimal re;
  private final BigDecimal im;

  public BigComplex(BigDecimal tre, BigDecimal tim) {
    int minScale = tre.scale() > 0 || tim.scale() > 0 ? 1 : 0;
    int reScaleStripped = tre.stripTrailingZeros().scale();
    int imScaleStripped = tim.stripTrailingZeros().scale();
    int reScale = Math.min(Utils.INSTANCE.getDEFAULT_SCALE(), Math.max(minScale, reScaleStripped));
    int imScale = Math.min(Utils.INSTANCE.getDEFAULT_SCALE(), Math.max(minScale, imScaleStripped));
    this.re = reScaleStripped > 0 ? tre.setScale(reScale, Utils.INSTANCE.getROUNDING_MODE()).stripTrailingZeros() : tre.setScale(reScale, Utils.INSTANCE.getROUNDING_MODE());
    this.im = imScaleStripped > 0 ? tim.setScale(imScale, Utils.INSTANCE.getROUNDING_MODE()).stripTrailingZeros() : tim.setScale(imScale, Utils.INSTANCE.getROUNDING_MODE());
  }

  public BigComplex(Number re, Number im) {
    this(Utils.INSTANCE.toBigDecimal(re), Utils.INSTANCE.toBigDecimal(im));
  }

  public BigComplex(Number re) {
    this(re, BigDecimal.ZERO);
  }

  /**
   * Return real part
   */
  public BigDecimal getRe() {
    return re;
  }

  /**
   * Return imaginary part
   */
  public BigDecimal getIm() {
    return im;
  }

  /**
   * Convert Number to BigComplex
   */
  public static BigComplex of(Number number) {
    return number instanceof BigComplex ? (BigComplex)number : new BigComplex(number);
  }

  /**
   * Addition
   */
  public BigComplex plus(Number other) {
    if (other instanceof BigComplex) {
      return new BigComplex(re.add(((BigComplex) other).getRe()), im.add(((BigComplex) other).getIm()));
    } else {
      return new BigComplex(re.add(Utils.INSTANCE.toBigDecimal(other)), im);
    }
  }

  /**
   * Subtraction
   */
  public BigComplex minus(Number other) {
    if (other instanceof BigComplex) {
      return new BigComplex(re.subtract(((BigComplex) other).getRe()), im.subtract(((BigComplex) other).getIm()));
    } else {
      return new BigComplex(re.subtract(Utils.INSTANCE.toBigDecimal(other)), im);
    }
  }

  /**
   * Multiplication
   *
   * (a + bi)(c + di) = (ac - bd) + (bc + ad)i
   **/
  public BigComplex multiply(Number other) {
    BigComplex o = of(other);
    BigDecimal a = this.re;
    BigDecimal b = this.im;
    BigDecimal c = o.re;
    BigDecimal d = o.im;
    return new BigComplex((a.multiply(c).subtract(b.multiply(d))), (b.multiply(c).add(a.multiply(d))));
  }

  /**
   * Square root of Complex number
   *
   * sqrt(a + bi) = +-(gamma + delta*i)
   *
   * gamma = sqrt((a + sqrt(a*a + b*b))/2)
   * delta = sign(b) * sqrt((-a + (a*a + b*b)/2)
   */
  public BigComplex sqrt() {
    double a   = this.re.doubleValue();
    double b   = this.im.doubleValue();
    int signum = this.im.signum();

    double s = Math.sqrt(a * a + b * b);
    double gamma = Math.sqrt((s+a)/2);
    double delta = signum * Math.sqrt((s-a)/2);
    return new BigComplex(gamma, delta);
  }

  /**
   * Division
   *
   * a + bi     ac + bd       bc - ad
   * ------ =  ----------  + --------- i
   * c + di    c*c + d*d     c*c + d*d
   */
  public BigComplex divide(Number other) {
    BigComplex o = of(other);
    BigDecimal a = this.re;
    BigDecimal b = this.im;
    BigDecimal c = o.re;
    BigDecimal d = o.im;
    BigDecimal real = a.multiply(c).add(b.multiply(d));
    BigDecimal imag = b.multiply(c).subtract(a.multiply(d));
    BigDecimal denom = c.multiply(c).add(d.multiply(d));
    return new BigComplex(real.divide(denom, Utils.INSTANCE.getDEFAULT_CONTEXT()),
                          imag.divide(denom, Utils.INSTANCE.getDEFAULT_CONTEXT()));
  }

  /**
   * Exponentiation
   *
   * z1^z2 = (a+bi)^(c+di) =:
   *
   * pow.re := (r^c)*exp(-d*t)*cos(c*t + d*ln(r))
   * pow.im := (r^c)*exp(-d*t)*sin(c*t + d*ln(r))
   *           |_____________|    |_____________|
   *                 A                  B
   *
   * where:
   *
   * r: magnitude(z1)
   * t: angle(z1)
   * c: z2.re
   * d: z2.im
   */
  public BigComplex expt(Number e) {
    BigDecimal c;
    BigDecimal d;
    if (e instanceof BigComplex) {
      c = ((BigComplex) e).getRe();
      d = ((BigComplex) e).getIm();
    } else {
      c = Utils.INSTANCE.toBigDecimal(e);
      d = BigDecimal.ZERO;
    }
    Number r = magnitude();
    Number t = angle();
    Number A = Multiplication.apply(Expt.expt(r, c), Exp.exp(Multiplication.apply(t, d.negate())));
    Number B = Addition.add(Multiplication.apply(c, t), Multiplication.apply(d, Log.log(r)));
    Number re = Multiplication.apply(A, Cos.cos(B));
    Number im = Multiplication.apply(A, Sin.sin(B));
    return new BigComplex(re, im);
  }

  /**
   * Natural logarithm of Complex number
   *
   * lnz = log(a + ib) = log(|a+bi|) + i*arg(a+bi)
   **/
  public BigComplex log() {
    Number re = Log.log(magnitude());
    Number im = angle();
    return new BigComplex(re, im);
  }

  /**
   * Magnitude (Absolute value, Modulus) of Complex number
   *
   * r = |z| = |a+bi| = sqrt(a^2 + b^2)
   **/
  public Number magnitude() {
    BigDecimal re = getRe();
    BigDecimal im = getIm();
    return Sqrt.sqrt(Addition.add(re.multiply(re), im.multiply(im)));
  }

  /**
   * Angle (Argument, Phase) of Complex number
   *
   * arg(z) = arg(a+bi) =:
   *
   * atan(b/y),      if x > 0
   * atan(b/y) + pi, if x < 0 and y >= 0
   * atan(b/y) - pi, if x < 0 and y <  0
   *  pi/2,          if x = 0 and y >  0
   * -pi/2,          if x = 0 and y <  0
   * undefined,      if x = 0 and y =  0
   **/
  public Number angle() {
    BigDecimal re = getRe();
    BigDecimal im = getIm();
    if (re.signum() == 0) {
      if (im.signum() > 0) {
        return Math.PI/2;
      } else if (im.signum() < 0) {
        return -Math.PI/2;
      } else {
        throw new ArithmeticException("Undefined for 0+0i");
      }
    } else if (re.signum() < 0) {
      double atan = Atan.atan(im.divide(re, Utils.INSTANCE.getDEFAULT_CONTEXT()));
      return (im.signum() >= 0) ? atan + Math.PI : atan - Math.PI;
    } else {
      return Atan.atan(im.divide(re, Utils.INSTANCE.getDEFAULT_CONTEXT()));
    }
  }

  @Override
  public int intValue() {
    throw new UnsupportedOperationException("Undefined for complex number!");
  }

  @Override
  public long longValue() {
    throw new UnsupportedOperationException("Undefined for complex number!");
  }

  @Override
  public float floatValue() {
    throw new UnsupportedOperationException("Undefined for complex number!");
  }

  @Override
  public double doubleValue() {
    throw new UnsupportedOperationException("Undefined for complex number!");
  }

  /**
   * Complex number is a zero if both real and imaginary parts are zeroes
   */
  public boolean isZero() {
    return re.signum() == 0 && im.signum() == 0;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    BigComplex that = (BigComplex) o;
    if (re != null ? !re.equals(that.re) : that.re != null) return false;
    return im != null ? im.equals(that.im) : that.im == null;
  }

  @Override
  public int hashCode() {
    int result = re != null ? re.hashCode() : 0;
    result = 31 * result + (im != null ? im.hashCode() : 0);
    return result;
  }

  @Override
  public String toString() {
    return (im.signum() <  0) ? (re + "-" + (im.negate()) + "i") : (re + "+" + im + "i");
  }
}
