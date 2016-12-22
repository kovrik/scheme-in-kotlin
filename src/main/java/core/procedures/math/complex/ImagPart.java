package core.procedures.math.complex;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {Number.class})
public class ImagPart extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "imag-part";
  }

  @Override
  public Number apply(Object... args) {
    if (args[0] instanceof SCMBigComplex) {
      return ((SCMBigComplex)args[0]).getIm();
    }
    return 0L;
  }
}
