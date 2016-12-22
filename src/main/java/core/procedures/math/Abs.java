package core.procedures.math;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigRational;
import core.scm.SCMClass;

import java.math.BigDecimal;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {SCMClass.Real.class})
public class Abs extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "abs";
  }

  @Override
  public Number apply(Object... args) {
    return abs((Number)args[0]);
  }

  public static Number abs(Number number) {
    if (number instanceof Long) {
      return Math.abs((Long) number);
    } else if (number instanceof Double) {
      return Math.abs((Double) number);
    } else if (number instanceof BigDecimal) {
      return ((BigDecimal) number).abs();
    } else {
      return ((SCMBigRational) number).abs();
    }
  }
}
