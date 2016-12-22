package core.procedures.math.complex;

import core.procedures.AFn;
import core.procedures.math.Addition;
import core.procedures.math.Multiplication;
import core.procedures.math.trigonometry.Cos;
import core.procedures.math.trigonometry.Sin;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;
import core.scm.SCMClass;

@FnArgs(minArgs = 2, maxArgs = 2, mandatoryArgsTypes = {SCMClass.Real.class, SCMClass.Real.class})
public class MakePolar extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "make-polar";
  }

  @Override
  public Number apply(Object... args) {
    /* (+ (* magnitude (cos angle)) (* magnitude (sin angle) 0+1i)) */
    Number m = (Number) args[0];
    Number a = (Number) args[1];
    return Addition.add(Multiplication.apply(m, Cos.cos(a)), SCMBigComplex.I.multiply(Sin.sin(a)).multiply(m));
  }
}
