package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.BigComplex;
import core.scm.BigRatio;
import core.utils.Utils;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Addition extends AFn {

  public Addition() {
    super(new FnArgsBuilder().rest(Number.class).build());
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
    if (Utils.isZero(first)) {
      return Utils.inexactnessTaint(second, first);
    }
    if (Utils.isZero(second)) {
      return Utils.inexactnessTaint(first, second);
    }
    /* Complex numbers*/
    if (first instanceof BigComplex) {
      return ((BigComplex) first).plus(second);
    }
    if (second instanceof BigComplex) {
      return ((BigComplex) second).plus(first);
    }
    /* Big Ratio numbers */
    if ((first instanceof BigRatio) && (second instanceof BigRatio)) {
      return ((BigRatio)first).plus((BigRatio)second);
    }
    if (first instanceof BigRatio) {
      if (second instanceof Long || second instanceof BigInteger) {
        return ((BigRatio) first).plus(BigRatio.valueOf(second.toString(), "1"));
      } else {
        first = first.doubleValue();
      }
    }
    if (second instanceof BigRatio) {
      if (first instanceof Long || first instanceof BigInteger) {
        return BigRatio.valueOf(first.toString(), "1").plus((BigRatio) second);
      } else {
        second = second.doubleValue();
      }
    }
    if (first instanceof Float && second instanceof Float) {
      float result = first.floatValue() + second.floatValue();
      if (Float.isNaN(result) || Float.isInfinite(result)) {
        return Utils.toBigDecimal(first).add(Utils.toBigDecimal(second));
      }
      return result;
    }
    if (first instanceof Double || second instanceof Double || first instanceof Float || second instanceof Float) {
      double result = first.doubleValue() + second.doubleValue();
      if (Double.isNaN(result) || Double.isInfinite(result)) {
        return Utils.toBigDecimal(first).add(Utils.toBigDecimal(second));
      }
      return result;
    }
    if (first instanceof BigDecimal || second instanceof BigDecimal) {
      return Utils.toBigDecimal(first).add(Utils.toBigDecimal(second));
    }
    if (first instanceof BigInteger || second instanceof BigInteger) {
      return Utils.toBigInteger(first).add(Utils.toBigInteger(second));
    }
    long f = first.longValue();
    long s = second.longValue();
    try {
      return Math.addExact(f, s);
    } catch (ArithmeticException e) {
      return BigDecimal.valueOf(f).add(BigDecimal.valueOf(s));
    }
  }
}
