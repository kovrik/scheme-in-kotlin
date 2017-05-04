package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMBigRational;
import core.scm.SCMClass;
import core.utils.NumberUtils;

import java.math.BigDecimal;
import java.math.BigInteger;

public final class Max extends AFn {

  public Max() {
    super(new FnArgsBuilder().min(1).mandatory(new Class[]{SCMClass.Real.class})
                             .rest(SCMClass.Real.class).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "max";
  }

  @Override
  public Number apply(Object... args) {
    if (args.length == 1) {
      return (Number) args[0];
    }
    Object result = args[0];
    for (Object arg : args) {
      result = max((Number) result, (Number) arg);
    }
    return (Number) result;
  }

  private Number max(Number first, Number second) {
    /* Big Rational numbers */
    if ((first instanceof SCMBigRational) && (second instanceof SCMBigRational)) {
      return ((SCMBigRational)first).compareTo((SCMBigRational)second) > 0 ? first : second;
    }
    if (first instanceof SCMBigRational) {
      first = first.doubleValue();
    }
    if (second instanceof SCMBigRational) {
      second = second.doubleValue();
    }
    if ((first instanceof Integer) && (second instanceof Integer)) {
      return Math.max((int)first, (int)second);
    }
    if ((first instanceof Long) && (second instanceof Long)) {
      return Math.max((long)first, (long)second);
    }
    if ((first instanceof Float) && (second instanceof Float)) {
      return Math.max((float)first, (float)second);
    }
    if ((first instanceof Double) && (second instanceof Double)) {
      return Math.max((double)first, (double) second);
    }
    if ((first instanceof BigInteger) && (second instanceof BigInteger)) {
      return ((BigInteger)first).max((BigInteger) second);
    }
    if ((first instanceof BigDecimal) && (second instanceof BigDecimal)) {
      return ((BigDecimal)first).max((BigDecimal) second);
    }
    if (first instanceof BigDecimal) {
      int i = ((BigDecimal) first).compareTo(NumberUtils.toBigDecimal(second));
      return (i < 0) ? second : first;
    }
    if (second instanceof BigDecimal) {
      int i = ((BigDecimal) second).compareTo(NumberUtils.toBigDecimal(first));
      return (i < 0) ? first : second;
    }
    if (first.doubleValue() == second.doubleValue()) {
      return first;
    } else if (first.doubleValue() > second.doubleValue()) {
      return first;
    }
    return second;
  }
}
