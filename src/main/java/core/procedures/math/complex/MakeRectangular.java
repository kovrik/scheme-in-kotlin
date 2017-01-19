package core.procedures.math.complex;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;
import core.scm.SCMClass;

@FnArgs(minArgs = 2, maxArgs = 2, mandatoryArgsTypes = {SCMClass.Real.class, SCMClass.Real.class})
public final class MakeRectangular extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "make-rectangular";
  }

  @Override
  public Number apply2(Object arg1, Object arg2) {
    /* (+ x (* y 0+1i)) */
    Number x = (Number) arg1;
    Number y = (Number) arg2;
    return SCMBigComplex.I.multiply(y).plus(x);
  }
}
