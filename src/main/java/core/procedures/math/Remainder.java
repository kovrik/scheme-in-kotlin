package core.procedures.math;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigRational;

import java.math.BigDecimal;

@FnArgs(args = {Long.class, Long.class})
public class Remainder extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "remainder";
  }

  @Override
  public Number invoke(Object... args) {
    return invoke((Number)args[0], (Number)args[1]);
  }

  public Number invoke(BigDecimal first, BigDecimal second) {
    if (second.compareTo(BigDecimal.ZERO) == 0) {
      throw new ArithmeticException(String.format("Error: (%s) undefined for 0", getName()));
    }
    return first.remainder(second);
  }

  public Number invoke(Number first, Number second) {
    if (first instanceof SCMBigRational) {
      if (!(((SCMBigRational)first).isDenominatorEqualToOne())) {
        throw new IllegalArgumentException(
          String.format("Error: (%s) bad argument type - not an integer: %s", getName(), first));
      }
      first = ((SCMBigRational) first).toBigDecimal();
    }
    if (second instanceof SCMBigRational) {
      if (!(((SCMBigRational) second).isDenominatorEqualToOne())) {
        throw new IllegalArgumentException(
          String.format("Error: (%s) bad argument type - not an integer: %s", getName(), second));
      }
      second = ((SCMBigRational) second).toBigDecimal();
    }

    if ((first instanceof BigDecimal) && (second instanceof BigDecimal)) {
      return invoke((BigDecimal)first, (BigDecimal)second);
    }
    if (first instanceof BigDecimal) {
      return invoke((BigDecimal)first, new BigDecimal(second.toString()));
    }
    if (second instanceof BigDecimal) {
      return invoke(new BigDecimal(first.toString()), new BigDecimal(second.toString()));
    }

    if ((first instanceof Double) || (second instanceof Double)) {
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

      double result = first.doubleValue() % second.doubleValue();
      // Don't want negative zero
      if (result == -0.0) {
        return Math.abs(result);
      }
      return result;
    } else if ((first instanceof Long) && (second instanceof Long)) {
      if (second.intValue() == 0) {
        throw new ArithmeticException(String.format("Error: (%s) undefined for 0", getName()));
      }
      return (Long)first % (Long)second;
    }
    throw new IllegalArgumentException(String.format("Wrong type argument to `%s`", getName()));
  }
}
