package core.procedures.math.complex;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMBigComplex;

public final class RealPart extends AFn {

  public RealPart() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{Number.class}));
  }

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
