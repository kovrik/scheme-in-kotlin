package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;

public final class Remainder extends AFn {

  private static final String NAME = "remainder";

  public Remainder() {
    super(new FnArgsBuilder().minArgs(2).maxArgs(2).mandatoryArgsTypes(new Class[]{Long.class, Long.class}));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return NAME;
  }

  @Override
  public Number apply2(Object arg1, Object arg2) {
    return apply((Number)arg1, (Number)arg2);
  }

  private static Number apply(BigDecimal first, BigDecimal second) {
    if (second.signum() == 0) {
      throw new ArithmeticException(String.format("%s: undefined for 0", NAME));
    }
    return first.remainder(second);
  }

  public static Number apply(Number first, Number second) {
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
      return apply((BigDecimal)first, NumberUtils.toBigDecimal(second));
    }
    if (second instanceof BigDecimal) {
      return apply(NumberUtils.toBigDecimal(first), (BigDecimal)second);
    }

    if ((first instanceof Double) || (second instanceof Double)) {
      if (second.intValue() == 0) {
        throw new ArithmeticException(String.format("%s: undefined for 0", NAME));
      }

      double result = first.doubleValue() % second.doubleValue();
      // Don't want negative zero
      if (result == -0.0) {
        return Math.abs(result);
      }
      return result;
    } else if ((first instanceof Long) && (second instanceof Long)) {
      if (second.intValue() == 0) {
        throw new ArithmeticException(String.format("%s: undefined for 0", NAME));
      }
      return (Long)first % (Long)second;
    }
    throw new IllegalArgumentException(String.format("Wrong type argument to `%s`", NAME));
  }
}
