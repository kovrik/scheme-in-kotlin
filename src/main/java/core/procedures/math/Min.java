package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMBigRational;
import core.scm.SCMClass;

import java.math.BigDecimal;
import java.util.Arrays;

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
    if ((first instanceof Long) && (second instanceof Long)) {
      return Math.min((Long)first, (Long)second);
    }
    if ((first instanceof BigDecimal) && (second instanceof BigDecimal)) {
      return ((BigDecimal)first).min((BigDecimal) second);
    }
    if (first instanceof BigDecimal) {
      return ((BigDecimal)first).min(new BigDecimal(second.toString()));
    }
    if (second instanceof BigDecimal) {
      return ((BigDecimal)second).min(new BigDecimal(first.toString()));
    }
    return Math.min(first.doubleValue(), second.doubleValue());
  }

  @Override
  public Number apply(Object... args) {
    return (Number) Arrays.stream(args).reduce(args[0], (f, s) -> min((Number)f, (Number)s));
  }
}
