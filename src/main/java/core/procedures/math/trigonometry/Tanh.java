package core.procedures.math.trigonometry;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;

@FnArgs(args = {Number.class})
public class Tanh extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "tanh";
  }

  @Override
  public Number apply(Object... args) {
    /* Special cases */
    if (NumberUtils.isZero(args[0])) {
      return 0L;
    }
    if (args[0] instanceof Long) {
      return Math.tanh((Long) args[0]);
    } else if (args[0] instanceof Double) {
      return Math.tanh((Double) args[0]);
    } else if (args[0] instanceof BigDecimal) {
      return tanh((BigDecimal)args[0]);
    } else if (args[0] instanceof SCMBigComplex) {
      return tanh((SCMBigComplex)args[0]);
    } else {
      return tanh(((SCMBigRational)args[0]).toBigDecimal());
    }
  }

  public static double tanh(BigDecimal bd) {
    double v = bd.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      return Math.tanh(v);
    }
  }

  public static SCMBigComplex tanh(SCMBigComplex c) {
    return Sinh.sinh(c).divide(Cosh.cosh(c));
  }
}
