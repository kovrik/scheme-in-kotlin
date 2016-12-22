package core.procedures.math.complex;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;
import core.scm.SCMClass;

@FnArgs(minArgs = 2, maxArgs = 2, mandatoryArgsTypes = {SCMClass.Real.class, SCMClass.Real.class})
public class MakeRectangular extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "make-rectangular";
  }

  @Override
  public Number apply(Object... args) {
    /* (+ x (* y 0+1i)) */
    Number x = (Number) args[0];
    Number y = (Number) args[1];
    return SCMBigComplex.I.multiply(y).plus(x);
  }
}
