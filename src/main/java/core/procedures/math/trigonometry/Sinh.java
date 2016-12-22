package core.procedures.math.trigonometry;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {Number.class})
public class Sinh extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "sinh";
  }

  @Override
  public Number apply(Object... args) {
    /* Special cases */
    if (NumberUtils.isZero(args[0])) {
      return 0L;
    }
    if (args[0] instanceof Long) {
      return Math.sinh((Long) args[0]);
    } else if (args[0] instanceof Double) {
      return Math.sinh((Double) args[0]);
    } else if (args[0] instanceof BigDecimal) {
      return sinh((BigDecimal)args[0]);
    } else if (args[0] instanceof SCMBigComplex) {
      return sinh((SCMBigComplex)args[0]);
    } else {
      return sinh(((SCMBigRational)args[0]).toBigDecimal());
    }
  }

  public static double sinh(BigDecimal bd) {
    double v = bd.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      return Math.sinh(v);
    }
  }

  /* sinh(x + yi) = sinh(x)*cos(y) + cosh(x)*sin(y)*i */
  public static Number sinh(SCMBigComplex c) {
    BigDecimal x = c.getRe();
    BigDecimal y = c.getIm();
    double re = sinh(x) * Cos.cos(y);
    double im = Cosh.cosh(x) * Sin.sin(y);
    if (Double.isInfinite(re) || Double.isNaN(re)) {
      return Double.NaN;
    }
    if (Double.isInfinite(im) || Double.isNaN(im)) {
      return Double.NaN;
    }
    return new SCMBigComplex(re, im);
  }
}
