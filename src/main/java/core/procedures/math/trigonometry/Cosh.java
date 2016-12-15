package core.procedures.math.trigonometry;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;

@FnArgs(args = {Number.class})
public class Cosh extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "cosh";
  }

  @Override
  public Number apply(Object... args) {
    /* Special cases */
    if (NumberUtils.isZero(args[0])) {
      return 1L;
    }
    if (args[0] instanceof Long) {
      return Math.cosh((Long) args[0]);
    } else if (args[0] instanceof Double) {
      return Math.cosh((Double) args[0]);
    } else if (args[0] instanceof BigDecimal) {
      return cosh((BigDecimal)args[0]);
    } else if (args[0] instanceof SCMBigComplex) {
      return cosh((SCMBigComplex)args[0]);
    } else {
      return cosh(((SCMBigRational)args[0]).toBigDecimal());
    }
  }

  public static double cosh(BigDecimal bd) {
    double v = bd.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      return Math.cosh(v);
    }
  }

  /* cosh(x + yi) = cosh(x)*cos(y) + sinh(x)*sin(y)*i */
  public static Number cosh(SCMBigComplex c) {
    BigDecimal x = c.getRe();
    BigDecimal y = c.getIm();
    double re = Cosh.cosh(x) * Cos.cos(y);
    double im = Sinh.sinh(x) * Sin.sin(y);
    if (Double.isInfinite(re) || Double.isNaN(re)) {
      return Double.NaN;
    }
    if (Double.isInfinite(im) || Double.isNaN(im)) {
      return Double.NaN;
    }
    return new SCMBigComplex(re, im);
  }
}
