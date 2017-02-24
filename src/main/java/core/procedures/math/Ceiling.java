package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMBigRational;
import core.scm.SCMClass;

import java.math.BigDecimal;

public final class Ceiling extends AFn {

  public Ceiling() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{SCMClass.Real.class}));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "ceiling";
  }

  @Override
  public Number apply1(Object arg) {
    if (arg instanceof Long) {
      return (Long) arg;
    } else if (arg instanceof Double) {
      return Math.ceil((Double) arg);
    } else if (arg instanceof BigDecimal) {
      return ((BigDecimal)arg).setScale(0, BigDecimal.ROUND_UP);
    } else {
      return ((SCMBigRational) arg).ceiling();
    }
  }
}
