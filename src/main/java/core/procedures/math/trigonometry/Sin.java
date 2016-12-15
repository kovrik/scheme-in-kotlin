package core.procedures.math.trigonometry;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.scm.SCMClass;

import java.math.BigDecimal;

@FnArgs(args = {SCMClass.Real.class})
public class Sin extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "sin";
  }

  @Override
  public Number apply(Object... args) {
    if (args[0] instanceof Long) {
      return Math.sin((Long) args[0]);
    } else if (args[0] instanceof Double) {
      return Math.sin((Double) args[0]);
    } else if (args[0] instanceof BigDecimal) {
      return sin((BigDecimal)args[0]);
    } else if (args[0] instanceof SCMBigComplex) {
      throw new UnsupportedOperationException("Not implemented yet!");
      // TODO sinh
    } else {
      return sin(((SCMBigRational)args[0]).toBigDecimal());
    }
  }

  public static double sin(BigDecimal bd) {
    double v = bd.doubleValue();
    if (Double.isInfinite(v) || Double.isNaN(v)) {
      return Double.NaN;
    } else {
      return Math.sin(v);
    }
  }
}
