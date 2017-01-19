package core.procedures.math;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;
import core.utils.BigDecimalMath;

import java.math.BigDecimal;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {Number.class})
public final class Sqrt extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "sqrt";
  }

  @Override
  public Number apply1(Object arg) {
    return sqrt((Number) arg);
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
      if (Double.isFinite(number.doubleValue())) {
        return Math.sqrt(number.doubleValue());
      }
      // FIXME Loss of precision
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
