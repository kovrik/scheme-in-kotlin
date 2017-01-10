package core.procedures.math;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigRational;
import core.scm.SCMClass;

import java.math.BigDecimal;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {SCMClass.Real.class})
public class Truncate extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "truncate";
  }

  @Override
  public Number apply1(Object arg) {
    if (arg instanceof Long) {
      return (Long) arg;
    } else if (arg instanceof Double) {
      if ((Double)arg < 0) {
        return Math.ceil((Double)arg);
      } else {
        return Math.floor((Double)arg);
      }
    } else if (arg instanceof BigDecimal) {
      BigDecimal bd = (BigDecimal) arg;
      if (bd.compareTo(BigDecimal.ZERO) < 0) {
        return bd.setScale(0, BigDecimal.ROUND_UP);
      } else {
        return bd.setScale(0, BigDecimal.ROUND_DOWN);
      }
    } else {
      return ((SCMBigRational) arg).truncate();
    }
  }
}
