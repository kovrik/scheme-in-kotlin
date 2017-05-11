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
    if (Utils.isZero(first)) {
      return Utils.inexactnessTaint(first, second);
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
    /* Big Ratio numbers */
    if ((first instanceof BigRatio) && (second instanceof BigRatio)) {
      return ((BigRatio)first).multiply((BigRatio)second);
    }
    if (first instanceof BigRatio) {
      if (Utils.isExact(second)) {
        return ((BigRatio) first).multiply(Utils.toBigInteger(second));
      } else {
        first = first.doubleValue();
      }
    }
    if (second instanceof BigRatio) {
      if (Utils.isExact(first)) {
        return ((BigRatio) second).multiply(Utils.toBigInteger(first));
      } else {
        second = second.doubleValue();
      }
    }
    if (first instanceof Float && second instanceof Float) {
      float result = first.floatValue() * second.floatValue();
      if (Float.isNaN(result) || Float.isInfinite(result)) {
        return Utils.toBigDecimal(first).multiply(Utils.toBigDecimal(second));
      }
      return result;
    }
    if (first instanceof Double || second instanceof Double || first instanceof Float || second instanceof Float) {
      double result = first.doubleValue() * second.doubleValue();
      if (Double.isNaN(result) || Double.isInfinite(result)) {
        return Utils.toBigDecimal(first).multiply(Utils.toBigDecimal(second));
      }
      return result;
    }
    if (first instanceof BigDecimal || second instanceof BigDecimal) {
      return Utils.toBigDecimal(first).multiply(Utils.toBigDecimal(second));
    }
    if (first instanceof BigInteger || second instanceof BigInteger) {
      return Utils.toBigInteger(first).multiply(Utils.toBigInteger(second));
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
