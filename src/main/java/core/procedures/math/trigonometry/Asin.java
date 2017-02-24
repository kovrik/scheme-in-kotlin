package core.procedures.math.trigonometry;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;

public final class Asin extends AFn {

  public Asin() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{Number.class}));
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
    if (NumberUtils.isZero(arg)) {
      return 0L;
    }
    if (arg instanceof Long) {
      double asin = Math.asin((Long) arg);
      if (Double.isNaN(asin)) {
        return asin(new SCMBigComplex((Number)arg));
      }
      return asin;
    } else if (arg instanceof Double) {
      double asin = Math.asin((Double) arg);
      if (Double.isNaN(asin)) {
        return asin(new SCMBigComplex((Number)arg));
      }
      return asin;
    } else if (arg instanceof BigDecimal) {
      return asin((BigDecimal)arg);
    } else if (arg instanceof SCMBigComplex) {
      return asin((SCMBigComplex)arg);
    } else {
      return asin(((SCMBigRational)arg).toBigDecimal());
    }
  }

  private static Number asin(BigDecimal bd) {
    double v = bd.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      double asin = Math.asin(v);
      if (Double.isNaN(asin)) {
        return asin(new SCMBigComplex(bd));
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
  private static Number asin(SCMBigComplex c) {
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
    return new SCMBigComplex(re, signum*im);
  }
}
