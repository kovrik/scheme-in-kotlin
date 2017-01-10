package core.procedures.math.complex;

import core.procedures.AFn;
import core.procedures.math.*;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {Number.class})
public class Magnitude extends AFn {

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
    if (arg instanceof SCMBigComplex) {
      return ((SCMBigComplex)arg).magnitude();
    }
    return Abs.abs((Number) arg);
  }
}
