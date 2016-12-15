package core.procedures.math.trigonometry;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;

@FnArgs(args = {Number.class})
public class Acos extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "acos";
  }

  @Override
  public Number apply(Object... args) {
    /* Special cases */
    if (NumberUtils.isZero(args[0])) {
      return 0L;
    }
    if (args[0] instanceof Long) {
      return Math.acos((Long) args[0]);
    } else if (args[0] instanceof Double) {
      return Math.acos((Double) args[0]);
    } else if (args[0] instanceof BigDecimal) {
      return acos((BigDecimal)args[0]);
    } else if (args[0] instanceof SCMBigComplex) {
      return acos((SCMBigComplex)args[0]);
    } else {
      return acos(((SCMBigRational)args[0]).toBigDecimal());
    }
  }

  public static double acos(BigDecimal bd) {
    double v = bd.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      return Math.acos(v);
    }
  }

  /* acos(a+bi) = acos(A) - ln(B + sqrt(B*B - 1))*i
   *
   * A = (sqrt((1+a)^2 + b^2) - sqrt((1-a)^2 + b^2))/2
   * B = (sqrt((1+a)^2 + b^2) + sqrt((1-a)^2 + b^2))/2
   *
   **/
  public static Number acos(SCMBigComplex c) {
    BigDecimal r = c.getRe();
    BigDecimal i = c.getIm();
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

    double re = Math.acos(A);
    if (Double.isInfinite(re) || Double.isNaN(re)) {
      return Double.NaN;
    }

    double im = Math.log(B + Math.sqrt(B*B - 1));
    if (Double.isInfinite(im) || Double.isNaN(im)) {
      return Double.NaN;
    }
    return new SCMBigComplex(re, -im);
  }
}
