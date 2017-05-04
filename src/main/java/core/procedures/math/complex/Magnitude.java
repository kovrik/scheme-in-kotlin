package core.procedures.math.complex;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.procedures.math.*;
import core.scm.BigComplex;

public final class Magnitude extends AFn {

  public Magnitude() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Number.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "magnitude";
  }

  @Override
  public Number apply1(Object arg) {
    if (arg instanceof BigComplex) {
      return ((BigComplex)arg).magnitude();
    }
    return Abs.abs((Number) arg);
  }
}
