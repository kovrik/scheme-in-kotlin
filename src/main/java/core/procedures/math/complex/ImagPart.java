package core.procedures.math.complex;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMBigComplex;

public final class ImagPart extends AFn {

  public ImagPart() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Number.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "imag-part";
  }

  @Override
  public Number apply1(Object arg) {
    if (arg instanceof SCMBigComplex) {
      return ((SCMBigComplex)arg).getIm();
    }
    return 0L;
  }
}
