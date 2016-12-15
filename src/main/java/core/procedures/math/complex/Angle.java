package core.procedures.math.complex;

import core.procedures.AFn;
import core.procedures.math.trigonometry.Atan;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;
import core.utils.NumberUtils;

import java.math.BigDecimal;

@FnArgs(args = {Number.class})
public class Angle extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "angle";
  }

  @Override
  public Number apply(Object... args) {
    return angle((Number) args[0]);
  }

  public static Number angle(Number number) {
    if (NumberUtils.isZero(number)) {
      throw new ArithmeticException("Undefined for 0");
    }
    return SCMBigComplex.of(number).angle();
  }
}
