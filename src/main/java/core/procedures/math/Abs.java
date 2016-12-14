package core.procedures.math;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigRational;

import java.math.BigDecimal;

@FnArgs(args = {Number.class})
public class Abs extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "abs";
  }

  @Override
  public Number apply(Object... args) {
    if (args[0] instanceof Long) {
      return Math.abs((Long) args[0]);
    } else if (args[0] instanceof Double) {
      return Math.abs((Double) args[0]);
    } else if (args[0] instanceof BigDecimal) {
      return ((BigDecimal) args[0]).abs();
    } else {
      return ((SCMBigRational) args[0]).abs();
    }
  }
}
