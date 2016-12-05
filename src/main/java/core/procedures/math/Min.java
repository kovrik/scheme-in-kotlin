package core.procedures.math;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMBigRational;

import java.math.BigDecimal;

public class Min extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "min";
  }

  public Number invoke(Number first, Number second) {
    if ((first instanceof SCMBigRational) && (second instanceof SCMBigRational)) {
      return ((SCMBigRational)first).compareTo((SCMBigRational)second) < 0 ? first : second;
    }
    if (first instanceof SCMBigRational) {
      first = first.doubleValue();
    }
    if (second instanceof SCMBigRational) {
      second = second.doubleValue();
    }
    if ((first instanceof Long) && (second instanceof Long)) {
      return Math.min((Long)first, (Long)second);
    }
    if ((first instanceof BigDecimal) && (second instanceof BigDecimal)) {
      return ((BigDecimal)first).min((BigDecimal) second);
    }
    if (first instanceof BigDecimal) {
      return ((BigDecimal)first).min(new BigDecimal(second.toString()));
    }
    if (second instanceof BigDecimal) {
      return ((BigDecimal)second).min(new BigDecimal(first.toString()));
    }

    return Math.min(first.doubleValue(), second.doubleValue());
  }

  @Override
  public Object invoke(Object... args) {
    if (args != null && args.length > 0) {
      if (args.length == 1) {
        return args[0];
      }
      Object result = args[0];
      if (!(result instanceof Number)) {
        throw new WrongTypeException("Number", result);
      }
      for (int i = 1; i < args.length; i++) {
        Number first = (Number)result;
        if (!(args[i] instanceof Number)) {
          throw new WrongTypeException("Number", args[i]);
        }
        result = invoke(first, (Number)args[i]);
      }
      return result;
    }
    throw new ArityException(args.length, 1, getName());
  }
}
