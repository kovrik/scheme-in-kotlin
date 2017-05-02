package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMBigRational;
import core.scm.SCMClass;
import core.utils.NumberUtils;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Min extends AFn {

  public Min() {
    super(new FnArgsBuilder().minArgs(1).mandatoryArgsTypes(new Class[]{SCMClass.Real.class})
                                        .restArgsType(SCMClass.Real.class));
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
    if ((first instanceof SCMBigRational) && (second instanceof SCMBigRational)) {
      return ((SCMBigRational)first).compareTo((SCMBigRational)second) < 0 ? first : second;
    }
    if (first instanceof SCMBigRational) {
      first = first.doubleValue();
    }
    if (second instanceof SCMBigRational) {
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
      int i = ((BigDecimal) first).compareTo(NumberUtils.toBigDecimal(second));
      return (i > 0) ? second : first;
    }
    if (second instanceof BigDecimal) {
      int i = ((BigDecimal) second).compareTo(NumberUtils.toBigDecimal(first));
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
