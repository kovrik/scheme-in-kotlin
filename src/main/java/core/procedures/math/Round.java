package core.procedures.math;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;
import java.math.MathContext;

@FnArgs(args = {Number.class})
public class Round extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "round";
  }

  @Override
  public Number invoke(Object... args) {
    if (args[0] instanceof Long) {
      return (Long) args[0];
    } else if (args[0] instanceof Double) {
      return Math.rint((Double) args[0]);
    } else if (args[0] instanceof BigDecimal) {
      BigDecimal number = (BigDecimal) args[0];
      if (number.scale() == 0) {
        return number.round(MathContext.UNLIMITED);
      } else {
        return number.round(NumberUtils.DEFAULT_CONTEXT);
      }
    } else {
      return ((SCMBigRational) args[0]).round();
    }
  }
}
