package core.procedures.math;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigRational;

import static core.utils.NumberUtils.E;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {Number.class})
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
  public Number apply(Object... args) {
    return exp((Number)args[0]);
  }

  public static Number exp(Number number) {
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
    return Expt.expt(E, number);
  }
}
