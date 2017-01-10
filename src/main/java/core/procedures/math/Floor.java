package core.procedures.math;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigRational;
import core.scm.SCMClass;

import java.math.BigDecimal;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {SCMClass.Real.class})
public class Floor extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "floor";
  }

  @Override
  public Number apply1(Object arg) {
    if (arg instanceof Long) {
      return (Long) arg;
    } else if (arg instanceof Double) {
      return Math.floor((Double) arg);
    } else if (arg instanceof BigDecimal) {
      return ((BigDecimal)arg).setScale(0, BigDecimal.ROUND_DOWN);
    } else {
      return ((SCMBigRational) arg).floor();
    }
  }
}
