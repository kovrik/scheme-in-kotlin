package core.procedures.math;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;

import java.math.BigDecimal;

public class Addition extends AFn {

  @Override
  public String getName() {
    return "+";
  }

  public Number invoke(Object first, Object second) {
    if (!(first instanceof Number)) {
      throw new WrongTypeException("Number", first);
    }
    if (!(second instanceof Number)) {
      throw new WrongTypeException("Number", second);
    }
    if ((first instanceof Long) && (second instanceof Long)) {
      return (Long)first + (Long)second;
    }
    if (first instanceof BigDecimal) {
      return ((BigDecimal)first).add(new BigDecimal(second.toString()));
    }
    if (second instanceof BigDecimal) {
      return ((BigDecimal)second).add(new BigDecimal(first.toString()));
    }
    double result = ((Number)first).doubleValue() + ((Number)second).doubleValue();
    if (Double.isNaN(result) || Double.isInfinite(result)) {
      return new BigDecimal(first.toString()).add(new BigDecimal(second.toString()));
    }
    return result;
  }

  @Override
  public Object invoke(Object... args) {
    Object result = 0L;
    if (args != null) {
      for (Object obj : args) {
        if (!(obj instanceof Number)) {
          throw new WrongTypeException("Number", obj);
        }
        result = invoke(result, obj);
      }
    }
    return result;
  }
}
