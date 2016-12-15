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
    if (NumberUtils.isZero(args[0])) {
      throw new ArithmeticException("Undefined for 0");
    }
    SCMBigComplex c = SCMBigComplex.of((Number) args[0]);
    BigDecimal re = c.getRe();
    BigDecimal im = c.getIm();
    if (re.compareTo(BigDecimal.ZERO) == 0) {
      if (im.signum() > 0) {
        return Math.PI/2;
      } else {
        return -Math.PI/2;
      }
    } else if (re.compareTo(BigDecimal.ZERO) < 0) {
      if (im.signum() >= 0) {
        return Atan.atan(im.divide(re, NumberUtils.DEFAULT_CONTEXT)) + Math.PI;
      } else {
        return Atan.atan(im.divide(re, NumberUtils.DEFAULT_CONTEXT)) - Math.PI;
      }
    } else {
      return Atan.atan(im.divide(re, NumberUtils.DEFAULT_CONTEXT));
    }
  }
}
