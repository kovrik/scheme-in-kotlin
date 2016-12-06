package core.procedures.math;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigRational;

import java.math.BigDecimal;

@FnArgs(args = {Number.class})
public class Truncate extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "truncate";
  }

  @Override
  public Number invoke(Object... args) {
    if (args[0] instanceof Long) {
      return (Long) args[0];
    } else if (args[0] instanceof Double) {
      Double arg = (Double) args[0];
      if (arg < 0) {
        return Math.ceil(arg);
      } else {
        return Math.floor(arg);
      }
    } else if (args[0] instanceof BigDecimal) {
      BigDecimal arg = (BigDecimal) args[0];
      if (arg.compareTo(BigDecimal.ZERO) < 0) {
        return arg.setScale(0, BigDecimal.ROUND_UP);
      } else {
        return arg.setScale(0, BigDecimal.ROUND_DOWN);
      }
    } else {
      return ((SCMBigRational) args[0]).truncate();
    }
  }
}
