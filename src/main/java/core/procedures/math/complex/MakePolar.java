package core.procedures.math.complex;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.procedures.math.Addition;
import core.procedures.math.Multiplication;
import core.procedures.math.trigonometry.Cos;
import core.procedures.math.trigonometry.Sin;
import core.scm.BigComplex;
import core.scm.Type;

public final class MakePolar extends AFn {

  public MakePolar() {
    super(new FnArgsBuilder().min(2).max(2).mandatory(new Class[]{Type.Real.class, Type.Real.class}).build());
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
    return Addition.Companion.add(Multiplication.Companion.apply(m, Cos.Companion.cos(a)), BigComplex.I.multiply(Sin.Companion.sin(a)).multiply(m));
  }
}
