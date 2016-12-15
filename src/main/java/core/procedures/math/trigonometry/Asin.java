package core.procedures.math.trigonometry;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;

@FnArgs(args = {Number.class})
public class Asin extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "asin";
  }

  @Override
  public Number apply(Object... args) {
    /* Special cases */
    if (NumberUtils.isZero(args[0])) {
      return 0L;
    }
    if (args[0] instanceof Long) {
      double asin = Math.asin((Long) args[0]);
      if (Double.isNaN(asin)) {
        return asin(new SCMBigComplex((Number)args[0]));
      }
      return asin;
    } else if (args[0] instanceof Double) {
      double asin = Math.asin((Double) args[0]);
      if (Double.isNaN(asin)) {
        return asin(new SCMBigComplex((Number)args[0]));
      }
      return asin;
    } else if (args[0] instanceof BigDecimal) {
      return asin((BigDecimal)args[0]);
    } else if (args[0] instanceof SCMBigComplex) {
      return asin((SCMBigComplex)args[0]);
    } else {
      return asin(((SCMBigRational)args[0]).toBigDecimal());
    }
  }

  public static Number asin(BigDecimal bd) {
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
  public static Number asin(SCMBigComplex c) {
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
