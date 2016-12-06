package core.procedures.math;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;

@FnArgs(args = {Number.class})
public class ToInexact extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "exact->inexact";
  }

  @Override
  public Number invoke(Object... args) {
    return toInexact(args[0]);
  }

  public static Number toInexact(Object o) {
    if (o instanceof SCMBigRational) {
      return ((SCMBigRational)o).toBigDecimalInexact();
    }
    if (o instanceof BigDecimal) {
      int scale = Math.max(1, ((BigDecimal)o).scale());
      return ((BigDecimal)o).setScale(scale, NumberUtils.ROUNDING_MODE);
    }
    return ((Number)o).doubleValue();
  }
}
