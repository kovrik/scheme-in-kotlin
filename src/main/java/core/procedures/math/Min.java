package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.BigRational;
import core.scm.Type;
import core.utils.Utils;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Min extends AFn {

  public Min() {
    super(new FnArgsBuilder().min(1).mandatory(new Class[]{Type.Real.class})
                             .rest(Type.Real.class).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "min";
  }

  @Override
  public Number apply(Object... args) {
    if (args.length == 1) {
      return (Number) args[0];
    }
    Object result = args[0];
    for (Object arg : args) {
      result = min((Number) result, (Number) arg);
    }
    return (Number) result;
  }

  private Number min(Number first, Number second) {
    if ((first instanceof BigRational) && (second instanceof BigRational)) {
      return ((BigRational)first).compareTo((BigRational)second) < 0 ? first : second;
    }
    if (first instanceof BigRational) {
      first = first.doubleValue();
    }
    if (second instanceof BigRational) {
      second = second.doubleValue();
    }
    if ((first instanceof Integer) && (second instanceof Integer)) {
      return Math.min((int)first, (int)second);
    }
    if ((first instanceof Long) && (second instanceof Long)) {
      return Math.min((long)first, (long)second);
    }
    if ((first instanceof Float) && (second instanceof Float)) {
      return Math.min((float)first, (float)second);
    }
    if ((first instanceof Double) && (second instanceof Double)) {
      return Math.min((double)first, (double) second);
    }
    if ((first instanceof BigInteger) && (second instanceof BigInteger)) {
      return ((BigInteger)first).min((BigInteger) second);
    }
    if ((first instanceof BigDecimal) && (second instanceof BigDecimal)) {
      return ((BigDecimal)first).min((BigDecimal) second);
    }
    if (first instanceof BigDecimal) {
      int i = ((BigDecimal) first).compareTo(Utils.toBigDecimal(second));
      return (i > 0) ? second : first;
    }
    if (second instanceof BigDecimal) {
      int i = ((BigDecimal) second).compareTo(Utils.toBigDecimal(first));
      return (i > 0) ? first : second;
    }
    if (first.doubleValue() == second.doubleValue()) {
      return first;
    } else if (first.doubleValue() < second.doubleValue()) {
      return first;
    }
    return second;
  }
}
