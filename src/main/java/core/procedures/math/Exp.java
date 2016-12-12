package core.procedures.math;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;

import static core.utils.NumberUtils.E;

@FnArgs(args = {Number.class})
public class Exp extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "exp";
  }

  @Override
  public Number invoke(Object... args) {
    return invoke((Number)args[0]);
  }

  public Number invoke(Number number) {
    if (number instanceof Double) {
      if ((Double)number == Double.NEGATIVE_INFINITY) {
        return 0L;
      }
      if ((Double.isNaN((Double) number)) || (Double.isInfinite((Double) number))) {
        return number;
      }
      return Math.exp(number.doubleValue());
    }
    if (number instanceof Long) {
      if (number.longValue() == 0) {
        return 1L;
      }
      return Math.exp(number.doubleValue());
    }
    if (number instanceof SCMBigRational) {
      /* Special cases */
      if (((SCMBigRational) number).isZero()) {
        return 1L;
      }
      if (((SCMBigRational) number).isOne()) {
        return Math.exp(1d);
      }
    }
    return Expt.invoke(E, number);
  }
}
