package core.procedures.math;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigRational;
import core.utils.BigDecimalMath;

import java.math.BigDecimal;

@FnArgs(args = {Number.class})
public class Sqrt extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "sqrt";
  }

  @Override
  public Number apply(Object... args) {
    if (args[0] instanceof Long) {
      return Math.sqrt((Long) args[0]);
    } else if (args[0] instanceof Double) {
      return Math.sqrt((Double) args[0]);
    } else if (args[0] instanceof BigDecimal) {
      if (Double.isInfinite(((BigDecimal) args[0]).doubleValue())) {
        return Double.POSITIVE_INFINITY;
      }
      return BigDecimalMath.sqrt((BigDecimal) args[0]);
    } else {
      return Math.sqrt(((SCMBigRational) args[0]).doubleValue());
    }
  }
}
