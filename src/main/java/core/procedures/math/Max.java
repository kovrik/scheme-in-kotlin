package core.procedures.math;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;

public class Max extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "max";
  }

  public Number apply(Number first, Number second) {
    /* Big Rational numbers */
    if ((first instanceof SCMBigRational) && (second instanceof SCMBigRational)) {
      return ((SCMBigRational)first).compareTo((SCMBigRational)second) > 0 ? first : second;
    }
    if (first instanceof SCMBigRational) {
      first = first.doubleValue();
    }
    if (second instanceof SCMBigRational) {
      second = second.doubleValue();
    }
    if ((first instanceof Long) && (second instanceof Long)) {
      return Math.max((Long)first, (Long)second);
    }

    if ((first instanceof BigDecimal) && (second instanceof BigDecimal)) {
      return ((BigDecimal)first).max((BigDecimal) second);
    }
    if (first instanceof BigDecimal) {
      return ((BigDecimal)first).max(new BigDecimal(second.toString()));
    }
    if (second instanceof BigDecimal) {
      return ((BigDecimal)second).max(new BigDecimal(first.toString()));
    }

    return Math.max(first.doubleValue(), second.doubleValue());
  }

  @Override
  public Object apply(Object... args) {
    if (args != null && args.length > 0) {
      if (args.length == 1) {
        return args[0];
      }
      Object result = args[0];
      if (!(NumberUtils.isReal(result))) {
        throw new WrongTypeException("Real", result);
      }
      for (int i = 1; i < args.length; i++) {
        Number first = (Number)result;
        if (!(NumberUtils.isReal(args[i]))) {
          throw new WrongTypeException("Real", args[i]);
        }
        result = apply(first, (Number)args[i]);
      }
      return result;
    }
    throw new ArityException(args.length, 1, getName());
  }
}
