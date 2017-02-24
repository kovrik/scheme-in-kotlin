package core.procedures.math.complex;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMBigComplex;
import core.utils.NumberUtils;

public final class Angle extends AFn {

  public Angle() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{Number.class}));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "angle";
  }

  @Override
  public Number apply1(Object arg) {
    return angle((Number) arg);
  }

  private Number angle(Number number) {
    if (NumberUtils.isZero(number)) {
      throw new ArithmeticException(getName() + ": undefined for 0");
    }
    return SCMBigComplex.of(number).angle();
  }
}
