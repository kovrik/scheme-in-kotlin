package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.BigComplex;
import core.scm.BigRational;
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
    /* Big Rational numbers */
    if ((first instanceof BigRational) && (second instanceof BigRational)) {
      return ((BigRational)first).plus((BigRational)second);
    }
    if (first instanceof BigRational) {
      if (second instanceof Long || second instanceof BigDecimal) {
        return ((BigRational) first).plus(BigRational.valueOf(second.toString(), "1"));
      } else {
        first = ((BigRational)first).doubleOrBigDecimalValue();
      }
    }
    if (second instanceof BigRational) {
      if (first instanceof Long || first instanceof BigDecimal) {
        return BigRational.valueOf(first.toString(), "1").plus((BigRational) second);
      } else {
        second = ((BigRational)second).doubleOrBigDecimalValue();
      }
    }
    if (first instanceof Float && second instanceof Float) {
      float result = first.floatValue() + second.floatValue();
      if (Float.isNaN(result) || Float.isInfinite(result)) {
        return new BigDecimal(first.toString()).add(new BigDecimal(second.toString()));
      }
      return result;
    }
    if (first instanceof Double || second instanceof Double || first instanceof Float || second instanceof Float) {
      double result = first.doubleValue() + second.doubleValue();
      if (Double.isNaN(result) || Double.isInfinite(result)) {
        return new BigDecimal(first.toString()).add(new BigDecimal(second.toString()));
      }
      return result;
    }
    if (first instanceof BigDecimal) {
      return ((BigDecimal)first).add(Utils.toBigDecimal(second));
    }
    if (second instanceof BigDecimal) {
      return ((BigDecimal) second).add(Utils.toBigDecimal(first));
    }
    if (first instanceof BigInteger) {
      return ((BigInteger)first).add(Utils.toBigInteger(second));
    }
    if (second instanceof BigInteger) {
      return Utils.toBigInteger(first).add((BigInteger) second);
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
