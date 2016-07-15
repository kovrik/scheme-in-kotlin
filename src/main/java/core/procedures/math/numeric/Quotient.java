package core.procedures.math.numeric;

import core.exceptions.ArityException;
import core.procedures.AFn;

import java.math.BigDecimal;
import java.math.BigInteger;

public class Quotient extends AFn implements INumericalOperation {

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
    throw new ArityException(args.length, 2, "quotient");
  }

  public Number zero() {
    throw new ArityException(0, 2, "quotient");
  }

  public Number apply(BigDecimal first, BigDecimal second) {
    if (second.compareTo(BigDecimal.ZERO) == 0) {
      throw new ArithmeticException("Error: (quotient) undefined for 0");
    }
    return first.divideToIntegralValue(second);
  }

  public Number apply(BigInteger first, BigInteger second) {
    if (second.compareTo(BigInteger.ZERO) == 0) {
      throw new ArithmeticException("Error: (quotient) undefined for 0");
    }
    return first.divide(second);
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
        throw new IllegalArgumentException("Error: (quotient) bad argument type - not an integer: " + first);
      }
      if (second.doubleValue() != Math.floor(second.doubleValue())) {
        throw new IllegalArgumentException("Error: (quotient) bad argument type - not an integer: " + second);
      }
      if (second.intValue() == 0) {
        throw new ArithmeticException("Error: (quotient) undefined for 0");
      }
      return (double)(first.longValue() / second.longValue());
    }
    if (second.intValue() == 0) {
      throw new ArithmeticException("Error: (quotient) undefined for 0");
    }
    return first.longValue() / second.longValue();
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
