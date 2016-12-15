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
      BigDecimal bd = (BigDecimal)args[0];
      double v = bd.doubleValue();
      if (Double.isInfinite(v) || Double.isNaN(v)) {
        return Double.NaN;
      } else {
        return Math.sin(v);
      }
    } else if (args[0] instanceof SCMBigComplex) {
      throw new UnsupportedOperationException("Not implemented yet!");
      // TODO sinh
    } else {
      BigDecimal bd = ((SCMBigRational)args[0]).toBigDecimal();
      double v = bd.doubleValue();
      if (Double.isInfinite(v) || Double.isNaN(v)) {
        return Double.NaN;
      } else {
        return Math.sin(v);
      }
    }
  }
}
