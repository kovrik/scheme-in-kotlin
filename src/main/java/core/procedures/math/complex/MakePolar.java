package core.procedures.math.complex;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.procedures.math.Addition;
import core.procedures.math.Multiplication;
import core.procedures.math.trigonometry.Cos;
import core.procedures.math.trigonometry.Sin;
import core.scm.SCMBigComplex;
import core.scm.SCMClass;

public final class MakePolar extends AFn {

  public MakePolar() {
    super(new FnArgsBuilder().minArgs(2).maxArgs(2).mandatoryArgsTypes(new Class[]{SCMClass.Real.class, SCMClass.Real.class}));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "make-polar";
  }

  @Override
  public Number apply2(Object arg1, Object arg2) {
    /* (+ (* magnitude (cos angle)) (* magnitude (sin angle) 0+1i)) */
    Number m = (Number) arg1;
    Number a = (Number) arg2;
    return Addition.add(Multiplication.apply(m, Cos.cos(a)), SCMBigComplex.I.multiply(Sin.sin(a)).multiply(m));
  }
}
