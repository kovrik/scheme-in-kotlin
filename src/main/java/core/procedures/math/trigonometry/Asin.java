package core.procedures.math.trigonometry;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.BigComplex;
import core.scm.BigRatio;
import core.utils.Utils;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Asin extends AFn {

  public Asin() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Number.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "asin";
  }

  @Override
  public Number apply1(Object arg) {
    /* Special cases */
    if (Utils.isZero(arg)) {
      return 0L;
    }
    if (arg instanceof BigDecimal) {
      return asin((BigDecimal)arg);
    } else if (arg instanceof BigInteger) {
      return asin((BigInteger)arg);
    } else if (arg instanceof BigComplex) {
      return asin((BigComplex)arg);
    } else if (arg instanceof BigRatio) {
      return asin(((BigRatio)arg).toBigDecimal());
    }
    double asin = Math.asin(((Number)arg).doubleValue());
    if (Double.isNaN(asin)) {
      return asin(new BigComplex((Number)arg));
    }
    return asin;
  }

  private static Number asin(BigDecimal bd) {
    double v = bd.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      double asin = Math.asin(v);
      if (Double.isNaN(asin)) {
        return asin(new BigComplex(bd));
      }
      return asin;
    }
  }

  private static Number asin(BigInteger bi) {
    double v = bi.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      double asin = Math.asin(v);
      if (Double.isNaN(asin)) {
        return asin(new BigComplex(bi));
      }
      return asin;
    }
  }

  /* asin(a+bi) = asin(A) + ln(B + sqrt(B*B - 1))*i
   *
   * A = (sqrt((1+a)^2 + b^2) - sqrt((1-a)^2 + b^2))/2
   * B = (sqrt((1+a)^2 + b^2) + sqrt((1-a)^2 + b^2))/2
   *
   **/
  private static Number asin(BigComplex c) {
    BigDecimal r = c.getRe();
    BigDecimal i = c.getIm();
    int signum;
    if (i.signum() == 0) {
      signum = -r.signum();
    } else {
      signum = i.signum();
    }
    double a = r.doubleValue();
    if (Double.isInfinite(a) || Double.isNaN(a)) {
      return Double.NaN;
    }
    double b = i.doubleValue();
    if (Double.isInfinite(b) || Double.isNaN(b)) {
      return Double.NaN;
    }

    double b2 = b*b;
    double L = Math.sqrt((1+a)*(1+a) + b2);
    double R = Math.sqrt((1-a)*(1-a) + b2);
    double A = (L - R)/2;
    double B = (L + R)/2;

    double re = Math.asin(A);
    if (Double.isInfinite(re) || Double.isNaN(re)) {
      return Double.NaN;
    }

    double im = Math.log(B + Math.sqrt(B*B - 1));
    if (Double.isInfinite(im) || Double.isNaN(im)) {
      return Double.NaN;
    }
    return new BigComplex(re, signum*im);
  }
}
