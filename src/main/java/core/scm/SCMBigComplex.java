package core.scm;

import java.math.BigDecimal;

/**
 * TODO Create SCMComplex class for small complex numbers
 */
public class SCMBigComplex extends Number implements ISCMClass {

  private final BigDecimal re;
  private final BigDecimal im;

  public SCMBigComplex(BigDecimal re, BigDecimal im) {
    this.re = re;
    this.im = im;
  }

  public SCMBigComplex(String re, String im) {
    this.re = new BigDecimal(re);
    this.im = new BigDecimal(im);
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
