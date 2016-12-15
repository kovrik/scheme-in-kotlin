package core.procedures.math.trigonometry;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.scm.SCMClass;

import java.math.BigDecimal;

@FnArgs(args = {SCMClass.Real.class})
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
    if (args[0] instanceof Long) {
      return Math.sinh((Long) args[0]);
    } else if (args[0] instanceof Double) {
      return Math.sinh((Double) args[0]);
    } else if (args[0] instanceof BigDecimal) {
      return sinh((BigDecimal)args[0]);
    } else if (args[0] instanceof SCMBigComplex) {
      throw new UnsupportedOperationException("Not implemented yet!");
      // TODO sinh
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
}
