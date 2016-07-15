package core.procedures.math.numeric;

import core.exceptions.ArityException;
import core.procedures.AFn;

import java.math.BigDecimal;
import java.math.BigInteger;

public class Remainder extends AFn implements INumericalOperation {

  @Override
  public Number invoke(Object... args) {
    if (args != null && args.length == 2) {
      if (!(args[0] instanceof Number)) {
        throw new IllegalArgumentException("Wrong argument type. Expected: Integer, actual: " + args[0].getClass().getSimpleName());
      }
      if (!(args[1] instanceof Number)) {
        throw new IllegalArgumentException("Wrong argument type. Expected: Integer, actual: " + args[1].getClass().getSimpleName());
      }
      return apply((Number)args[0], (Number)args[1]);
    }
    throw new ArityException(args.length, 2, "remainder");
  }

  public Number zero() {
    throw new ArityException(0, 2, "remainder");
  }

  public Number apply(BigInteger first, BigInteger second) {
    if (second.compareTo(BigInteger.ZERO) == 0) {
      throw new ArithmeticException("Error: (remainder) undefined for 0");
    }
    return first.remainder(second);
  }

  public Number apply(BigDecimal first, BigDecimal second) {
    if (second.compareTo(BigDecimal.ZERO) == 0) {
      throw new ArithmeticException("Error: (remainder) undefined for 0");
    }
    return first.remainder(second);
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

    if ((first instanceof BigInteger) && (second instanceof BigInteger)) {
      return apply((BigInteger)first, (BigInteger)second);
    }
    if (first instanceof BigInteger) {
      return apply((BigInteger)first, new BigInteger(second.toString()));
    }
    if (second instanceof BigInteger) {
      return apply(new BigInteger(first.toString()), new BigInteger(second.toString()));
    }

    if ((first instanceof Double) || (second instanceof Double)) {
      // check if they are integral
      if (first.doubleValue() != Math.floor(first.doubleValue())) {
        throw new IllegalArgumentException("Error: (remainder) bad argument type - not an integer: " + first);
      }
      if (second.doubleValue() != Math.floor(second.doubleValue())) {
        throw new IllegalArgumentException("Error: (remainder) bad argument type - not an integer: " + second);
      }
      if (second.intValue() == 0) {
        throw new ArithmeticException("Error: (remainder) undefined for 0");
      }

      double result = first.doubleValue() % second.doubleValue();
      // Don't want negative zero
      if (result == -0.0) {
        return Math.abs(result);
      }
      return result;
    } else if ((first instanceof Long) && (second instanceof Long)) {
      if (second.intValue() == 0) {
        throw new ArithmeticException("Error: (remainder) undefined for 0");
      }
      return (Long)first % (Long)second;
    }
    throw new IllegalArgumentException("Wrong type argument to `remainder`");
  }

  public Object apply(Object first, Object second) {
    return invoke(first, second);
  }

  @Override
  public Object call() throws Exception {
    return invoke();
  }

  @Override
  public void run() {
    invoke();
  }
}
