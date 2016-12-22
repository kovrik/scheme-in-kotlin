package core.procedures.math;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {SCMBigRational.class})
public class Numerator extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "numerator";
  }

  @Override
  public Number apply(Object... args) {
    return numerator(args[0]);
  }

  public static Number numerator(Object o) {
    boolean isExact = NumberUtils.isExact(o);
    Number exact;
    if (isExact) {
      exact = (Number)o;
    } else {
      exact = ToExact.toExact(o);
    }
    if (exact instanceof SCMBigRational) {
      BigDecimal result = new BigDecimal(((SCMBigRational) exact).getNumerator());
      if (!isExact) {
        return result.setScale(1, NumberUtils.ROUNDING_MODE);
      }
      return result;
    }
    return exact;
  }
}
