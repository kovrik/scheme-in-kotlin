package core.procedures.math;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;
import java.math.BigInteger;

public class Subtraction extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "-";
  }

  @Override
  public Object apply(Object... args) {
    if (args == null || args.length == 0) {
      throw new ArityException(0, "-");
    }
    if (args.length == 1) {
      if (args[0] instanceof BigDecimal) {
        return ((BigDecimal)args[0]).negate();
      }
      if (args[0] instanceof SCMBigRational) {
        return ((SCMBigRational)args[0]).negate();
      }
      return apply(0L, args[0]);
    }
    Object result = args[0];
    for (int i = 1; i < args.length; i++) {
      if (!(args[0] instanceof Number)) {
        throw new WrongTypeException("Number", args[0]);
      }
      result = apply((Number)result, (Number)args[i]);
    }
    return result;
  }

  public Number apply(Number first, Number second) {
    /* Special cases */
    if (NumberUtils.isZero(second)) {
      return NumberUtils.inexactnessTaint(first, second);
    }
    /* Big Rational numbers */
    if ((first instanceof SCMBigRational) && (second instanceof SCMBigRational)) {
      return ((SCMBigRational)first).minus((SCMBigRational)second);
    }
    if (first instanceof SCMBigRational) {
      if (second instanceof Long) {
        return ((SCMBigRational) first).minus(new SCMBigRational(new BigInteger(second.toString()), BigInteger.ONE));
      } else {
        first = first.doubleValue();
      }
    }
    if (second instanceof SCMBigRational) {
      if (first instanceof Long) {
        return ((SCMBigRational) second).minus(new SCMBigRational(new BigInteger(first.toString()), BigInteger.ONE));
      } else {
        second = second.doubleValue();
      }
    }

    if ((first instanceof Long) && (second instanceof Long)) {
      try {
        return Math.subtractExact((Long)first, (Long)second);
      } catch (ArithmeticException e) {
        return new BigDecimal(first.toString()).subtract(new BigDecimal(second.toString()));
      }
    }
    if (first instanceof BigDecimal) {
      return ((BigDecimal)first).subtract(new BigDecimal(second.toString()));
    }
    if (second instanceof BigDecimal) {
      return new BigDecimal(first.toString()).subtract((BigDecimal)second);
    }
    double result = first.doubleValue() - second.doubleValue();
    if (Double.isNaN(result) || Double.isInfinite(result)) {
      return new BigDecimal(first.toString()).subtract(new BigDecimal(second.toString()));
    }
    return result;
  }
}
