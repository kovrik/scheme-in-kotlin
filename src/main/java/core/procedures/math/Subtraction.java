package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.BigComplex;
import core.scm.BigRatio;
import core.utils.Utils;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Subtraction extends AFn {

  public Subtraction() {
    super(new FnArgsBuilder().min(1).rest(Number.class).build());
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
      if (args[0] instanceof BigInteger) {
        return ((BigInteger)args[0]).negate();
      }
      if (args[0] instanceof BigRatio) {
        return ((BigRatio)args[0]).negate();
      }
      if (args[0] instanceof Long) {
        try {
          return Math.negateExact(((Long)args[0]));
        } catch (ArithmeticException e) {
          return BigInteger.valueOf((long)args[0]).negate();
        }
      }
      if (args[0] instanceof Integer) {
        try {
          return Math.negateExact(((Integer) args[0]));
        } catch (ArithmeticException e) {
          return Math.negateExact(((Integer)args[0]).longValue());
        }
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
    if (Utils.isZero(second)) {
      return Utils.inexactnessTaint(first, second);
    }
    /* Complex numbers*/
    if (first instanceof BigComplex) {
      return ((BigComplex) first).minus(second);
    }
    if (second instanceof BigComplex) {
      return new BigComplex(first).minus(second);
    }
    /* Big Rational numbers */
    if ((first instanceof BigRatio) && (second instanceof BigRatio)) {
      return ((BigRatio)first).minus((BigRatio)second);
    }
    if (first instanceof BigRatio) {
      if (second instanceof Long) {
        return ((BigRatio) first).minus(BigRatio.valueOf(second.toString(), "1"));
      } else {
        first = first.doubleValue();
      }
    }
    if (second instanceof BigRatio) {
      if (first instanceof Long) {
        return BigRatio.valueOf(first.toString(), "1").minus((BigRatio) second);
      } else {
        second = second.doubleValue();
      }
    }
    if (first instanceof Float && second instanceof Float) {
      float result = first.floatValue() - second.floatValue();
      if (Float.isNaN(result) || Float.isInfinite(result)) {
        return Utils.toBigDecimal(first).subtract(Utils.toBigDecimal(second));
      }
      return result;
    }
    if (first instanceof Double || second instanceof Double || first instanceof Float || second instanceof Float) {
      double result = first.doubleValue() - second.doubleValue();
      if (Double.isNaN(result) || Double.isInfinite(result)) {
        return Utils.toBigDecimal(first).subtract(Utils.toBigDecimal(second));
      }
      return result;
    }
    if (first instanceof BigDecimal) {
      return ((BigDecimal)first).subtract(Utils.toBigDecimal(second));
    }
    if (second instanceof BigDecimal) {
      return ((BigDecimal) second).subtract(Utils.toBigDecimal(first));
    }
    if (first instanceof BigInteger) {
      return ((BigInteger)first).subtract(Utils.toBigInteger(second));
    }
    if (second instanceof BigInteger) {
      return Utils.toBigInteger(first).subtract((BigInteger) second);
    }
    long f = first.longValue();
    long s = second.longValue();
    try {
      return Math.subtractExact(f, s);
    } catch (ArithmeticException e) {
      return BigDecimal.valueOf(f).subtract(BigDecimal.valueOf(s));
    }
  }
}
