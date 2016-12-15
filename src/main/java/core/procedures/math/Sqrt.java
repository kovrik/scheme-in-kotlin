package core.procedures.math;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;
import core.scm.SCMBigRational;
import core.utils.BigDecimalMath;

import java.math.BigDecimal;

@FnArgs(args = {Number.class})
public class Sqrt extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "sqrt";
  }

  @Override
  public Number apply(Object... args) {
    return sqrt((Number) args[0]);
  }

  public static Number sqrt(Number number) {
    if (number instanceof Long) {
      return Math.sqrt((Long) number);
    } else if (number instanceof Double) {
      return Math.sqrt((Double) number);
    } else if (number instanceof BigDecimal) {
      if (Double.isInfinite(number.doubleValue())) {
        return Double.POSITIVE_INFINITY;
      }
      return BigDecimalMath.sqrt((BigDecimal) number);
    } else if (number instanceof SCMBigComplex) {
      if (((SCMBigComplex)number).getIm().compareTo(BigDecimal.ZERO) == 0) {
        return sqrt(((SCMBigComplex)number).getRe());
      }
      return ((SCMBigComplex)number).sqrt();
    } else {
      return Math.sqrt(number.doubleValue());
    }
  }
}
