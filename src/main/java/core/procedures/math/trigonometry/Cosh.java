package core.procedures.math.trigonometry;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.scm.SCMClass;

import java.math.BigDecimal;

@FnArgs(args = {SCMClass.Real.class})
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
    if (args[0] instanceof Long) {
      return Math.cosh((Long) args[0]);
    } else if (args[0] instanceof Double) {
      return Math.cosh((Double) args[0]);
    } else if (args[0] instanceof BigDecimal) {
      return cosh((BigDecimal)args[0]);
    } else if (args[0] instanceof SCMBigComplex) {
      throw new UnsupportedOperationException("Not implemented yet!");
      // TODO cosh
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
}
