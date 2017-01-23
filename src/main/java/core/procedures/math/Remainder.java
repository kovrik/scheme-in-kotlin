package core.procedures.math;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigRational;

import java.math.BigDecimal;

@FnArgs(minArgs = 2, maxArgs = 2, mandatoryArgsTypes = {Long.class, Long.class})
public final class Remainder extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "remainder";
  }

  @Override
  public Number apply2(Object arg1, Object arg2) {
    return apply((Number)arg1, (Number)arg2);
  }

  private Number apply(BigDecimal first, BigDecimal second) {
    if (second.compareTo(BigDecimal.ZERO) == 0) {
      throw new ArithmeticException(String.format("%s: undefined for 0", getName()));
    }
    return first.remainder(second);
  }

  private Number apply(Number first, Number second) {
    if (first instanceof SCMBigRational) {
      first = ((SCMBigRational) first).toBigDecimal();
    }
    if (second instanceof SCMBigRational) {
      second = ((SCMBigRational) second).toBigDecimal();
    }

    if ((first instanceof BigDecimal) && (second instanceof BigDecimal)) {
      return apply((BigDecimal)first, (BigDecimal)second);
    }
    if (first instanceof BigDecimal) {
      return apply((BigDecimal)first, new BigDecimal(second.toString()));
    }
    if (second instanceof BigDecimal) {
      return apply(new BigDecimal(first.toString()), new BigDecimal(second.toString()));
    }

    if ((first instanceof Double) || (second instanceof Double)) {
      if (second.intValue() == 0) {
        throw new ArithmeticException(String.format("%s: undefined for 0", getName()));
      }

      double result = first.doubleValue() % second.doubleValue();
      // Don't want negative zero
      if (result == -0.0) {
        return Math.abs(result);
      }
      return result;
    } else if ((first instanceof Long) && (second instanceof Long)) {
      if (second.intValue() == 0) {
        throw new ArithmeticException(String.format("%s: undefined for 0", getName()));
      }
      return (Long)first % (Long)second;
    }
    throw new IllegalArgumentException(String.format("Wrong type argument to `%s`", getName()));
  }
}
