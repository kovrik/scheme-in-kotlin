package core.procedures.math.trigonometry;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;

@FnArgs(args = {Number.class})
public class Atan extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "atan";
  }

  @Override
  public Number apply(Object... args) {
    /* Special cases */
    if (NumberUtils.isZero(args[0])) {
      return 0L;
    }
    if (args[0] instanceof Long) {
      return Math.atan((Long) args[0]);
    } else if (args[0] instanceof Double) {
      return Math.atan((Double) args[0]);
    } else if (args[0] instanceof BigDecimal) {
      return atan((BigDecimal)args[0]);
    } else if (args[0] instanceof SCMBigComplex) {
      return atan((SCMBigComplex)args[0]);
    } else {
      return atan(((SCMBigRational)args[0]).toBigDecimal());
    }
  }

  public static double atan(BigDecimal bd) {
    double v = bd.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      return Math.atan(v);
    }
  }

  public static Number atan(SCMBigComplex c) {
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

    double a2 = a*a;
    double b2 = b*b;
    // FIXME
//    double re = Math.atan((2*a)/(1 - a2 - b2))/2 ;
//    if (Double.isInfinite(re) || Double.isNaN(re)) {
//      return Double.NaN;
//    }
//
//    double im = (Math.log((a2 + (1+b)*(1+b)) / (a2 + (1-b)*(1-b)) ))/4;
//    if (Double.isInfinite(im) || Double.isNaN(im)) {
//      return Double.NaN;
//    }
//    return new SCMBigComplex(re, -im);
    throw new UnsupportedOperationException("Not implemented yet!");
  }
}
