package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.BigComplex;
import core.scm.BigRatio;
import core.utils.Utils;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Multiplication extends AFn {

  public Multiplication() {
    super(new FnArgsBuilder().rest(Number.class).build());
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
    if (Utils.isZero(first) || Utils.isZero(second)) {
      return 0L;
    }
    if (Utils.isOne(first)) {
      return Utils.inexactnessTaint(second, first);
    }
    if (Utils.isOne(second)) {
      return Utils.inexactnessTaint(first, second);
    }
    /* Complex numbers*/
    if (first instanceof BigComplex) {
      return ((BigComplex) first).multiply(second);
    }
    if (second instanceof BigComplex) {
      return ((BigComplex) second).multiply(first);
    }
    /* Big Rational numbers */
    if ((first instanceof BigRatio) && (second instanceof BigRatio)) {
      return ((BigRatio)first).multiply((BigRatio)second);
    }
    if (first instanceof BigRatio) {
      if (second instanceof Long) {
        return ((BigRatio) first).multiply(BigRatio.valueOf(second.toString(), "1"));
      } else {
        first = first.doubleValue();
      }
    }
    if (second instanceof BigRatio) {
      if (first instanceof Long) {
        return ((BigRatio) second).multiply(BigRatio.valueOf(first.toString(), "1"));
      } else {
        second = second.doubleValue();
      }
    }
    if (first instanceof Float && second instanceof Float) {
      float result = first.floatValue() * second.floatValue();
      if (Float.isNaN(result) || Float.isInfinite(result)) {
        return new BigDecimal(first.toString()).multiply(new BigDecimal(second.toString()));
      }
      return result;
    }
    if (first instanceof Double || second instanceof Double || first instanceof Float || second instanceof Float) {
      double result = first.doubleValue() * second.doubleValue();
      if (Double.isNaN(result) || Double.isInfinite(result)) {
        return new BigDecimal(first.toString()).multiply(new BigDecimal(second.toString()));
      }
      return result;
    }
    if (first instanceof BigDecimal) {
      return ((BigDecimal)first).multiply(Utils.toBigDecimal(second));
    }
    if (second instanceof BigDecimal) {
      return ((BigDecimal) second).multiply(Utils.toBigDecimal(first));
    }
    if (first instanceof BigInteger) {
      return ((BigInteger)first).multiply(Utils.toBigInteger(second));
    }
    if (second instanceof BigInteger) {
      return Utils.toBigInteger(first).multiply((BigInteger)second);
    }
    long f = first.longValue();
    long s = second.longValue();
    try {
      return Math.multiplyExact(f, s);
    } catch (ArithmeticException e) {
      return BigDecimal.valueOf(f).multiply(BigDecimal.valueOf(s));
    }
  }
}
