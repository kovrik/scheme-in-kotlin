package core.procedures.math;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;

import java.math.BigDecimal;

public class Quotient extends AFn {

  @Override
  public String getName() {
    return "quotient";
  }

  @Override
  public Number invoke(Object... args) {
    if (args != null && args.length == 2) {
      if (!(args[0] instanceof Number)) {
        throw new WrongTypeException("Integer", args[0]);
      }
      if (!(args[1] instanceof Number)) {
        throw new WrongTypeException("Integer", args[1]);
      }
      return invoke((Number)args[0], (Number)args[1]);
    }
    throw new ArityException(args.length, 2, getName());
  }

  public Number invoke(BigDecimal first, BigDecimal second) {
    if (second.compareTo(BigDecimal.ZERO) == 0) {
      throw new ArithmeticException(String.format("Error: (%s) undefined for 0", getName()));
    }
    return first.divideToIntegralValue(second);
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
      return (double)(first.longValue() / second.longValue());
    }
    if (second.intValue() == 0) {
      throw new ArithmeticException(String.format("Error: (%s) undefined for 0", getName()));
    }
    return first.longValue() / second.longValue();
  }
}
