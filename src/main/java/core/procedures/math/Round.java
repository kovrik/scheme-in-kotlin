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
    return round((Number) args[0]);
  }

  public static Number round(Number number) {
    if (number instanceof Long) {
      return number;
    } else if (number instanceof Double) {
      return Math.rint((Double) number);
    } else if (number instanceof BigDecimal) {
      BigDecimal bd = (BigDecimal) number;
      if (bd.scale() == 0) {
        return bd.round(MathContext.UNLIMITED);
      } else {
        return bd.round(NumberUtils.DEFAULT_CONTEXT);
      }
    } else {
      return ((SCMBigRational) number).round();
    }
  }
}
