package core.procedures.math.complex;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {Number.class})
public final class RealPart extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "real-part";
  }

  @Override
  public Number apply1(Object arg) {
    if (arg instanceof SCMBigComplex) {
      return ((SCMBigComplex)arg).getRe();
    }
    return (Number)arg;
  }
}
