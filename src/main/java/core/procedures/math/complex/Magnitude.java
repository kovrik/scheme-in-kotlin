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
  public Number apply(Object... args) {
    if (args[0] instanceof SCMBigComplex) {
      return ((SCMBigComplex)args[0]).magnitude();
    }
    return Abs.abs((Number) args[0]);
  }
}
