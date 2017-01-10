package core.procedures.math;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;

@FnArgs(minArgs = 2, maxArgs = 2, mandatoryArgsTypes = {Long.class, Long.class})
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
  public Number apply2(Object arg1, Object arg2) {
    /* Special cases */
    if (NumberUtils.isOne(arg2)) {
      return NumberUtils.inexactnessTaint((Number)arg1, (Number) arg2);
    }
    if (NumberUtils.isZero(arg2)) {
      throw new ArithmeticException(String.format("Error: (%s) undefined for 0", getName()));
    }
    return apply((Number) arg1, (Number) arg2);
  }

  public Number apply(BigDecimal first, BigDecimal second) {
    int scale = Math.max(first.scale(), second.scale());
    if (scale > 0) {
      return first.divide(second, NumberUtils.DEFAULT_CONTEXT).setScale(0, NumberUtils.ROUNDING_MODE)
                  .setScale(1, NumberUtils.ROUNDING_MODE);
    } else {
      return first.divideToIntegralValue(second).setScale(scale, NumberUtils.ROUNDING_MODE);
    }
  }

  public Number apply(Number first, Number second) {
    if ((first instanceof BigDecimal) && (second instanceof BigDecimal)) {
      return apply((BigDecimal)first, (BigDecimal)second);
    }
    if (first instanceof BigDecimal) {
      return apply((BigDecimal)first, new BigDecimal(second.toString()));
    }
    if (second instanceof BigDecimal) {
      return apply(new BigDecimal(first.toString()), new BigDecimal(second.toString()));
    }

    if ((first instanceof Double) || (second instanceof Double) ||
        (first instanceof SCMBigRational) || (second instanceof SCMBigRational)) {

      return apply(new BigDecimal(first.toString()), new BigDecimal(second.toString()));
    }
    return first.longValue() / second.longValue();
  }
}
