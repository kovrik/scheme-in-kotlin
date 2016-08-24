package core.procedures.math;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;

import java.math.BigDecimal;

public class Multiplication extends AFn {

  @Override
  public String getName() {
    return "*";
  }

  @Override
  public Object invoke(Object... args) {
    Object result = 1L;
    if (args != null) {
      for (Object obj : args) {
        if (!(obj instanceof Number)) {
          throw new WrongTypeException("Number", obj);
        }
        result = invoke((Number)result, (Number)obj);
      }
    }
    return result;
  }

  public Number invoke(Number first, Number second) {
    if ((first instanceof Long) && (second instanceof Long)) {
      try {
        return Math.multiplyExact((Long) first, (Long) second);
      } catch (ArithmeticException e) {
        return new BigDecimal(first.toString()).multiply(new BigDecimal(second.toString()));
      }
    }
    if (first instanceof BigDecimal) {
      return ((BigDecimal)first).multiply(new BigDecimal(second.toString()));
    }
    if (second instanceof BigDecimal) {
      return ((BigDecimal)second).multiply(new BigDecimal(first.toString()));
    }
    double result = first.doubleValue() * second.doubleValue();
    if (Double.isNaN(result) || Double.isInfinite(result)) {
      return new BigDecimal(first.toString()).multiply(new BigDecimal(second.toString()));
    }
    return result;
  }
}
