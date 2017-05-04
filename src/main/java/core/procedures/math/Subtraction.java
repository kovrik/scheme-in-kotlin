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
      if (args[0] instanceof SCMBigRational) {
        return ((SCMBigRational)args[0]).negate();
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
        return ((SCMBigRational) first).minus(SCMBigRational.valueOf(second.toString(), "1"));
      } else {
        first = first.doubleValue();
      }
    }
    if (second instanceof SCMBigRational) {
      if (first instanceof Long) {
        return SCMBigRational.valueOf(first.toString(), "1").minus((SCMBigRational) second);
      } else {
        second = second.doubleValue();
      }
    }
    if (first instanceof Float && second instanceof Float) {
      float result = first.floatValue() - second.floatValue();
      if (Float.isNaN(result) || Float.isInfinite(result)) {
        return new BigDecimal(first.toString()).subtract(new BigDecimal(second.toString()));
      }
      return result;
    }
    if (first instanceof Double || second instanceof Double || first instanceof Float || second instanceof Float) {
      double result = first.doubleValue() - second.doubleValue();
      if (Double.isNaN(result) || Double.isInfinite(result)) {
        return new BigDecimal(first.toString()).subtract(new BigDecimal(second.toString()));
      }
      return result;
    }
    if (first instanceof BigDecimal) {
      return ((BigDecimal)first).subtract(NumberUtils.toBigDecimal(second));
    }
    if (second instanceof BigDecimal) {
      return ((BigDecimal) second).subtract(NumberUtils.toBigDecimal(first));
    }
    if (first instanceof BigInteger) {
      return ((BigInteger)first).subtract(new BigInteger(second.toString()));
    }
    if (second instanceof BigInteger) {
      return ((BigInteger)second).subtract(new BigInteger(first.toString()));
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
