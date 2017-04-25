package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Multiplication extends AFn {

  public Multiplication() {
    super(new FnArgsBuilder().restArgsType(Number.class));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "*";
  }

  @Override
  public Number apply(Object... args) {
    switch (args.length) {
      case 0:  return 1L;
      case 1:  return (Number)args[0];
      default: {
        Object result = 1L;
        for (Object arg : args) {
          result = apply((Number) result, (Number) arg);
        }
        return (Number)result;
      }
    }
  }

  public static Number apply(Number first, Number second) {
    /* Special cases */
    if (NumberUtils.isZero(first) || NumberUtils.isZero(second)) {
      return 0L;
    }
    if (NumberUtils.isOne(first)) {
      return NumberUtils.inexactnessTaint(second, first);
    }
    if (NumberUtils.isOne(second)) {
      return NumberUtils.inexactnessTaint(first, second);
    }
    /* Complex numbers*/
    if (first instanceof SCMBigComplex) {
      return ((SCMBigComplex) first).multiply(second);
    }
    if (second instanceof SCMBigComplex) {
      return new SCMBigComplex(first).multiply(second);
    }
    /* Big Rational numbers */
    if ((first instanceof SCMBigRational) && (second instanceof SCMBigRational)) {
      return ((SCMBigRational)first).multiply((SCMBigRational)second);
    }
    if (first instanceof SCMBigRational) {
      if (second instanceof Long) {
        return ((SCMBigRational) first).multiply(new SCMBigRational(new BigInteger(second.toString()), BigInteger.ONE));
      } else {
        first = ((SCMBigRational)first).doubleOrBigDecimalValue();
      }
    }
    if (second instanceof SCMBigRational) {
      if (first instanceof Long) {
        return ((SCMBigRational) second).multiply(new SCMBigRational(new BigInteger(first.toString()), BigInteger.ONE));
      } else {
        second = ((SCMBigRational)second).doubleOrBigDecimalValue();
      }
    }
    if ((first instanceof Long) && (second instanceof Long)) {
      try {
        return Math.multiplyExact((Long) first, (Long) second);
      } catch (ArithmeticException e) {
        return BigDecimal.valueOf((Long)first).multiply(BigDecimal.valueOf((Long)second));
      }
    }
    if (first instanceof BigDecimal) {
      return ((BigDecimal)first).multiply(NumberUtils.toBigDecimal(second));
    }
    if (second instanceof BigDecimal) {
      return ((BigDecimal)second).multiply(NumberUtils.toBigDecimal(first));
    }
    double result = first.doubleValue() * second.doubleValue();
    if (Double.isNaN(result) || Double.isInfinite(result)) {
      return new BigDecimal(first.toString()).multiply(new BigDecimal(second.toString()));
    }
    return result;
  }
}
