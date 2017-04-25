package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Subtraction extends AFn {

  public Subtraction() {
    super(new FnArgsBuilder().minArgs(1).restArgsType(Number.class));
  }

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
    if (args.length == 1) {
      if (args[0] instanceof BigDecimal) {
        return ((BigDecimal)args[0]).negate();
      }
      if (args[0] instanceof SCMBigRational) {
        return ((SCMBigRational)args[0]).negate();
      }
      if (args[0] instanceof Long) {
        return Math.negateExact(((Long)args[0]));
      }
      return subtract(0L, (Number)args[0]);
    }
    Object result = args[0];
    for (int i = 1; i < args.length; i++) {
      result = subtract((Number)result, (Number)args[i]);
    }
    return result;
  }

  private Number subtract(Number first, Number second) {
    /* Special cases */
    if (NumberUtils.isZero(second)) {
      return NumberUtils.inexactnessTaint(first, second);
    }
    /* Complex numbers*/
    if (first instanceof SCMBigComplex) {
      return ((SCMBigComplex) first).minus(second);
    }
    if (second instanceof SCMBigComplex) {
      return new SCMBigComplex(first).minus(second);
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
        return new SCMBigRational(new BigInteger(first.toString()), BigInteger.ONE).minus((SCMBigRational) second);
      } else {
        second = second.doubleValue();
      }
    }

    if ((first instanceof Long) && (second instanceof Long)) {
      try {
        return Math.subtractExact((Long)first, (Long)second);
      } catch (ArithmeticException e) {
        return BigDecimal.valueOf((Long)first).subtract(BigDecimal.valueOf((Long)second));
      }
    }
    if (first instanceof BigDecimal) {
      return ((BigDecimal)first).subtract(NumberUtils.toBigDecimal(second));
    }
    if (second instanceof BigDecimal) {
      return NumberUtils.toBigDecimal(first).subtract((BigDecimal)second);
    }
    double result = first.doubleValue() - second.doubleValue();
    if (Double.isNaN(result) || Double.isInfinite(result)) {
      return new BigDecimal(first.toString()).subtract(new BigDecimal(second.toString()));
    }
    return result;
  }
}
