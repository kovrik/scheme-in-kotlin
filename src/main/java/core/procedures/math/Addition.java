package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Addition extends AFn {

  public Addition() {
    super(new FnArgsBuilder().restArgsType(Number.class));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "+";
  }

  @Override
  public Number apply(Object... args) {
    switch (args.length) {
      case 0:  return 0L;
      case 1:  return (Number)args[0];
      default: {
        Object result = 0L;
        for (Object arg : args) {
          result = add((Number) result, (Number) arg);
        }
        return (Number) result;
      }
    }
  }

  public static Number add(Number first, Number second) {
    /* Special cases */
    if (NumberUtils.isZero(first)) {
      return NumberUtils.inexactnessTaint(second, first);
    }
    if (NumberUtils.isZero(second)) {
      return NumberUtils.inexactnessTaint(first, second);
    }
    /* Complex numbers*/
    if (first instanceof SCMBigComplex) {
      return ((SCMBigComplex) first).plus(second);
    }
    if (second instanceof SCMBigComplex) {
      return new SCMBigComplex(first).plus(second);
    }
    /* Big Rational numbers */
    if ((first instanceof SCMBigRational) && (second instanceof SCMBigRational)) {
      return ((SCMBigRational)first).plus((SCMBigRational)second);
    }
    if (first instanceof SCMBigRational) {
      if (second instanceof Long || second instanceof BigDecimal) {
        return ((SCMBigRational) first).plus(new SCMBigRational(new BigInteger(second.toString()), BigInteger.ONE));
      } else {
        first = ((SCMBigRational)first).doubleOrBigDecimalValue();
      }
    }
    if (second instanceof SCMBigRational) {
      if (first instanceof Long || first instanceof BigDecimal) {
        return new SCMBigRational(new BigInteger(first.toString()), BigInteger.ONE).plus((SCMBigRational) second);
      } else {
        second = ((SCMBigRational)second).doubleOrBigDecimalValue();
      }
    }

    if ((first instanceof Long) && (second instanceof Long)) {
      try {
        return Math.addExact((Long)first, (Long)second);
      } catch (ArithmeticException e) {
        return new BigDecimal((Long)first).add(new BigDecimal((Long)second));
      }
    }
    if (first instanceof BigDecimal) {
      return ((BigDecimal)first).add(new BigDecimal(second.toString()));
    }
    if (second instanceof BigDecimal) {
      return new BigDecimal(first.toString()).add((BigDecimal) second);
    }
    double result = first.doubleValue() + second.doubleValue();
    if (Double.isNaN(result) || Double.isInfinite(result)) {
      return new BigDecimal(first.toString()).add(new BigDecimal(second.toString()));
    }
    return result;
  }
}
