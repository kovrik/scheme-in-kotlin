package core.procedures.math;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;

@FnArgs(args = {Number.class, Number.class})
public class Quotient extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "quotient";
  }

  @Override
  public Number invoke(Object... args) {
    return invoke((Number) args[0], (Number) args[1]);
  }

  public Number invoke(BigDecimal first, BigDecimal second) {
    if (second.compareTo(BigDecimal.ZERO) == 0) {
      throw new ArithmeticException(String.format("Error: (%s) undefined for 0", getName()));
    }
    int scale = Math.max(first.scale(), second.scale());
    if (scale > 0) {
      return first.divide(second, NumberUtils.DEFAULT_CONTEXT).setScale(0, NumberUtils.ROUNDING_MODE)
                  .setScale(1, NumberUtils.ROUNDING_MODE);
    } else {
      return first.divideToIntegralValue(second).setScale(scale, NumberUtils.ROUNDING_MODE);
    }
  }

  public Number invoke(Number first, Number second) {

    if ((first instanceof BigDecimal) && (second instanceof BigDecimal)) {
      return invoke((BigDecimal)first, (BigDecimal)second);
    }
    if (first instanceof BigDecimal) {
      return invoke((BigDecimal)first, new BigDecimal(second.toString()));
    }
    if (second instanceof BigDecimal) {
      return invoke(new BigDecimal(first.toString()), new BigDecimal(second.toString()));
    }

    if ((first instanceof Double) || (second instanceof Double) ||
        (first instanceof SCMBigRational) || (second instanceof SCMBigRational)) {

      // check if they are integral
      if (first.doubleValue() != Math.floor(first.doubleValue())) {
        throw new IllegalArgumentException(String.format("Error: (%s) bad argument type - not an integer: %s", getName(), first));
      }
      if (second.doubleValue() != Math.floor(second.doubleValue())) {
        throw new IllegalArgumentException(String.format("Error: (%s) bad argument type - not an integer: %s", getName(), second));
      }
      if (second.intValue() == 0) {
        throw new ArithmeticException(String.format("Error: (%s) undefined for 0", getName()));
      }
      return invoke(new BigDecimal(first.toString()), new BigDecimal(second.toString()));
    }
    if (second.intValue() == 0) {
      throw new ArithmeticException(String.format("Error: (%s) undefined for 0", getName()));
    }
    return first.longValue() / second.longValue();
  }
}
