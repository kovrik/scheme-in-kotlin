package core.procedures.math;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigRational;
import core.scm.SCMClass;

import java.math.BigDecimal;
import java.util.Arrays;

@FnArgs(minArgs = 1, mandatoryArgsTypes = {SCMClass.Real.class}, restArgsType = SCMClass.Real.class)
public final class Max extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "max";
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
    if ((first instanceof Long) && (second instanceof Long)) {
      return Math.max((Long)first, (Long)second);
    }

    if ((first instanceof BigDecimal) && (second instanceof BigDecimal)) {
      return ((BigDecimal)first).max((BigDecimal) second);
    }
    if (first instanceof BigDecimal) {
      return ((BigDecimal)first).max(new BigDecimal(second.toString()));
    }
    if (second instanceof BigDecimal) {
      return ((BigDecimal)second).max(new BigDecimal(first.toString()));
    }
    return Math.max(first.doubleValue(), second.doubleValue());
  }

  @Override
  public Number apply(Object... args) {
    return (Number) Arrays.stream(args).reduce(args[0], (f, s) -> max((Number)f, (Number)s));
  }
}
