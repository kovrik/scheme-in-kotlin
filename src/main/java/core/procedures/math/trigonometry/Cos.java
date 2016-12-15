package core.procedures.math.trigonometry;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;

import java.math.BigDecimal;

@FnArgs(args = {Number.class})
public class Cos extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "cos";
  }

  @Override
  public Number apply(Object... args) {
    if (args[0] instanceof Long) {
      return Math.cos((Long) args[0]);
    } else if (args[0] instanceof Double) {
      return Math.cos((Double) args[0]);
    } else if (args[0] instanceof BigDecimal) {
      return cos((BigDecimal)args[0]);
    } else if (args[0] instanceof SCMBigComplex) {
      return Cosh.cosh((SCMBigComplex)args[0]);
    } else {
      return cos(((SCMBigRational)args[0]).toBigDecimal());
    }
  }

  public static double cos(BigDecimal bd) {
    double v = bd.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      return Math.cos(v);
    }
  }
}
