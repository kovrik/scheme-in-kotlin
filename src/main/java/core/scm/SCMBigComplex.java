package core.scm;

import core.utils.NumberUtils;

import java.math.BigDecimal;

/**
 * TODO Create SCMComplex class for small complex numbers
 */
public class SCMBigComplex extends Number implements ISCMClass {

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
