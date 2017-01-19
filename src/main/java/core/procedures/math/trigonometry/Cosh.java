package core.procedures.math.trigonometry;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {Number.class})
public final class Cosh extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "cosh";
  }

  @Override
  public Number apply1(Object arg) {
    /* Special cases */
    if (NumberUtils.isZero(arg)) {
      return 1L;
    }
    if (arg instanceof Long) {
      return Math.cosh((Long) arg);
    } else if (arg instanceof Double) {
      return Math.cosh((Double) arg);
    } else if (arg instanceof BigDecimal) {
      return cosh((BigDecimal)arg);
    } else if (arg instanceof SCMBigComplex) {
      return cosh((SCMBigComplex)arg);
    } else {
      return cosh(((SCMBigRational)arg).toBigDecimal());
    }
  }

  static double cosh(BigDecimal bd) {
    double v = bd.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      return Math.cosh(v);
    }
  }

  /* cosh(x + yi) = cosh(x)*cos(y) + sinh(x)*sin(y)*i */
  static Number cosh(SCMBigComplex c) {
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
