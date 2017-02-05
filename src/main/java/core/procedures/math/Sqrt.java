package core.procedures.math;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigComplex;

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
    if (number instanceof SCMBigComplex) {
      if (((SCMBigComplex)number).getIm().compareTo(BigDecimal.ZERO) == 0) {
        return sqrt(((SCMBigComplex)number).getRe());
      }
      return ((SCMBigComplex)number).sqrt();
    }
    return Math.sqrt(number.doubleValue());
  }
}
