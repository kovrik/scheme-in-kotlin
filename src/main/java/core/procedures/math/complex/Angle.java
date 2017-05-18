package core.procedures.math.complex;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.BigComplex;
import core.utils.Utils;

public final class Angle extends AFn {

  public Angle() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Number.class}).build());
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
    if (Utils.INSTANCE.isZero(number)) {
      throw new ArithmeticException(getName() + ": undefined for 0");
    }
    return BigComplex.of(number).angle();
  }
}
