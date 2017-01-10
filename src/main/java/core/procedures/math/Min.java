package core.procedures.math;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigRational;
import core.scm.SCMClass;

import java.math.BigDecimal;

@FnArgs(minArgs = 1, mandatoryArgsTypes = {SCMClass.Real.class}, restArgsType = {SCMClass.Real.class})
public class Min extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "min";
  }

  public Number apply(Number first, Number second) {
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
    if (args.length == 1) {
      return (Number) args[0];
    }
    Number result = (Number) args[0];
    for (int i = 1; i < args.length; i++) {
      result = apply(result, (Number)args[i]);
    }
    return result;
  }
}
