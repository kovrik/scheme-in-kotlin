package core.procedures.math;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigRational;

import java.math.BigDecimal;

@FnArgs(args = {Number.class})
public class Ceiling extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "ceiling";
  }

  @Override
  public Number apply(Object... args) {
    if (args[0] instanceof Long) {
      return (Long) args[0];
    } else if (args[0] instanceof Double) {
      return Math.ceil((Double) args[0]);
    } else if (args[0] instanceof BigDecimal) {
      BigDecimal arg = (BigDecimal) args[0];
      return arg.setScale(0, BigDecimal.ROUND_UP);
    } else {
      return ((SCMBigRational) args[0]).ceiling();
    }
  }
}
