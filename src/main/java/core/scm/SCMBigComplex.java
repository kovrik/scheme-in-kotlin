package core.scm;

import core.procedures.math.Addition;
import core.procedures.math.Exp;
import core.procedures.math.Expt;
import core.procedures.math.Log;
import core.procedures.math.Multiplication;
import core.procedures.math.Sqrt;
import core.procedures.math.trigonometry.Atan;
import core.procedures.math.trigonometry.Cos;
import core.procedures.math.trigonometry.Sin;
import core.utils.NumberUtils;

import java.math.BigDecimal;

/**
 * TODO Create SCMComplex class for small complex numbers
 */
public class SCMBigComplex extends Number implements ISCMClass {

  public static final SCMBigComplex IM = new SCMBigComplex(BigDecimal.ZERO, BigDecimal.ONE);

  private static final BigDecimal HALF = new BigDecimal("0.5");

  // TODO use generic Number instead
  private final BigDecimal re;
  private final BigDecimal im;

  public SCMBigComplex(BigDecimal re, BigDecimal im) {
    this.re = re;
    this.im = im;
  }

  public SCMBigComplex(Number re, Number im) {
    /* FIXME Support rational re and im parts! */
    if (re instanceof SCMBigRational) {
      this.re = ((SCMBigRational)re).toBigDecimal();
    } else {
      this.re = new BigDecimal(re.toString());
    }
    if (im instanceof SCMBigRational) {
      this.im = ((SCMBigRational)im).toBigDecimal();
    } else {
      this.im = new BigDecimal(im.toString());
    }
  }

  public SCMBigComplex(Number re) {
    this(re, BigDecimal.ZERO);
  }

  public static SCMBigComplex of(Number number) {
    if (number instanceof SCMBigComplex) {
      return (SCMBigComplex) number;
    } else {
      return new SCMBigComplex(number);
    }
  }

  // TODO Optimize all arithmetic operations

  public SCMBigComplex plus(Number other) {
    if (other instanceof SCMBigComplex) {
      return new SCMBigComplex(re.add(((SCMBigComplex) other).getRe()), im.add(((SCMBigComplex) other).getIm()));
    } else {
      BigDecimal bd = NumberUtils.toBigDecimal(other);
      return new SCMBigComplex(re.add(bd), im);
    }
  }

  public SCMBigComplex minus(Number other) {
    if (other instanceof SCMBigComplex) {
      return new SCMBigComplex(re.subtract(((SCMBigComplex) other).getRe()), im.subtract(((SCMBigComplex) other).getIm()));
    } else {
      BigDecimal bd = NumberUtils.toBigDecimal(other);
      return new SCMBigComplex(re.subtract(bd), im);
    }
  }

  /* (a + bi)(c + di) = (ac - bd) + (bc + ad)i  */
  public SCMBigComplex multiply(Number other) {
    SCMBigComplex o;
    if (other instanceof SCMBigComplex) {
      o = (SCMBigComplex) other;
    } else {
      o = new SCMBigComplex(other);
    }
    BigDecimal a = this.re;
    BigDecimal b = this.im;
    BigDecimal c = o.re;
    BigDecimal d = o.im;
    return new SCMBigComplex((a.multiply(c).subtract(b.multiply(d))), (b.multiply(c).add(a.multiply(d))));
  }

  /*
   * sqrt(a + bi) = +-(gamma + delta*i)
   *
   * gamma = sqrt((a + sqrt(a*a + b*b))/2)
   * delta = sign(b)*sqrt((-a + (a*a + b*b)/2)
   */
  // FIXME Use sqrt for BigDecimal, not Double
  public SCMBigComplex sqrt() {
    double a   = this.re.doubleValue();
    double b   = this.im.doubleValue();
    int signum = this.im.signum();

    double s = Math.sqrt(a * a + b * b);
    double gamma = Math.sqrt((s+a)/2);
    double delta = signum * Math.sqrt((s-a)/2);
    return new SCMBigComplex(gamma, delta);
  }

  public SCMBigComplex sqrtBig() {
    BigDecimal a   = this.re;
    BigDecimal b   = this.im;
    int signum = this.im.signum();

    Number s = Sqrt.sqrt(a.multiply(a).add(b.multiply(b)));
    Number gamma = Sqrt.sqrt(Multiplication.apply(Addition.add(s, a), HALF));
    Number delta = Multiplication.apply(signum, Sqrt.sqrt(Multiplication.apply(Addition.add(s, Multiplication.apply(-1, a)), HALF)));
    return new SCMBigComplex(gamma, delta);
  }

  /* a + bi     ac + bd       bc - ad
   * ------ =  ----------  + --------- i
   * c + di    c*c + d*d     c*c + d*d
   */
  public SCMBigComplex divide(Number other) {
    SCMBigComplex o;
    if (other instanceof SCMBigComplex) {
      o = (SCMBigComplex) other;
    } else {
      o = new SCMBigComplex(other);
    }
    BigDecimal a = this.re;
    BigDecimal b = this.im;
    BigDecimal c = o.re;
    BigDecimal d = o.im;
    BigDecimal real = a.multiply(c).add(b.multiply(d));
    BigDecimal imag = b.multiply(c).subtract(a.multiply(d));
    BigDecimal denom = c.multiply(c).add(d.multiply(d));
    return new SCMBigComplex(real.divide(denom, NumberUtils.DEFAULT_CONTEXT),
                             imag.divide(denom, NumberUtils.DEFAULT_CONTEXT));
  }

  /*
   * r : magnitude(x)
   * t : angle(x)
   * c : y.re
   * d : y.im
   *
   * pow.re := (r^c)*exp(-d*t)*cos(c*t + d*ln(r))
   * pow.im := (r^c)*exp(-d*t)*sin(c*t + d*ln(r))
   *           |_____________|    |_____________|
   *                 A                  B
   */
  public SCMBigComplex expt(Number e) {
    BigDecimal c;
    BigDecimal d;
    if (e instanceof SCMBigComplex) {
      c = ((SCMBigComplex) e).getRe();
      d = ((SCMBigComplex) e).getIm();
    } else {
      if (e instanceof SCMBigRational) {
        c = ((SCMBigRational) e).toBigDecimal();
      } else {
        c = new BigDecimal(e.toString());
      }
      d = BigDecimal.ZERO;
    }
    // FIXME precision loss
    Number r = magnitude();
    Number t = angle();
    Number A = Multiplication.apply(Expt.expt(r, c), Exp.exp(Multiplication.apply(t, d.negate())));
    Number B = Addition.add(Multiplication.apply(c, t), Multiplication.apply(d, Log.log(r)));
    Number re = Multiplication.apply(A, Cos.cos(B));
    Number im = Multiplication.apply(A, Sin.sin(B));
    return new SCMBigComplex(re, im);
  }

  /* log(a + ib) = log(sqrt(a*a + b*b)) + atan(b/a)i */
  public SCMBigComplex log() {
    BigDecimal a = getRe();
    BigDecimal b = getIm();
    Number log = Log.log(magnitude());
    double atan = Atan.atan(b.divide(a, NumberUtils.DEFAULT_CONTEXT));
    return new SCMBigComplex(log, atan);
  }

  public Number magnitude() {
    BigDecimal re = getRe();
    BigDecimal im = getIm();
    return Sqrt.sqrt(Addition.add(re.multiply(re), im.multiply(im)));
  }

  public Number angle() {
    BigDecimal re = getRe();
    BigDecimal im = getIm();
    if (re.compareTo(BigDecimal.ZERO) == 0) {
      if (im.signum() > 0) {
        return Math.PI/2;
      } else {
        return -Math.PI/2;
      }
    } else if (re.compareTo(BigDecimal.ZERO) < 0) {
      if (im.signum() >= 0) {
        return Atan.atan(im.divide(re, NumberUtils.DEFAULT_CONTEXT)) + Math.PI;
      } else {
        return Atan.atan(im.divide(re, NumberUtils.DEFAULT_CONTEXT)) - Math.PI;
      }
    } else {
      return Atan.atan(im.divide(re, NumberUtils.DEFAULT_CONTEXT));
    }
  }

  public BigDecimal getRe() {
    return re;
  }

  public BigDecimal getIm() {
    return im;
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

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.COMPLEX;
  }

  public boolean isZero() {
    return re.compareTo(BigDecimal.ZERO) == 0 && im.compareTo(BigDecimal.ZERO) == 0;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    SCMBigComplex that = (SCMBigComplex) o;
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
    if (im.compareTo(BigDecimal.ZERO) <  0) {
      return re + "-" + (im.negate()) + "i";
    } else {
      return re + "+" + im + "i";
    }
  }
}