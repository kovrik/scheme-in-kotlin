package core.procedures.math;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;
import java.math.BigInteger;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {SCMBigRational.class})
public class Denominator extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "denominator";
  }

  @Override
  public Number apply1(Object arg) {
    return denominator(arg);
  }

  private static Number denominator(Object o) {
    boolean isExact = NumberUtils.isExact(o);
    Number exact;
    if (isExact) {
      exact = (Number)o;
    } else {
      exact = ToExact.toExact(o);
    }
    if (exact instanceof SCMBigRational) {
      BigDecimal result = new BigDecimal(((SCMBigRational) exact).getDenominator());
      if (!isExact) {
        return result.setScale(1, NumberUtils.ROUNDING_MODE);
      }
      return result;
    }
    if (exact instanceof Long || exact instanceof Integer) {
      return 1L;
    }
    if (exact instanceof Double || exact instanceof Float) {
      return 1d;
    }
    if (exact instanceof BigInteger) {
      return BigInteger.ONE;
    }
    if (exact instanceof BigDecimal) {
      if (((BigDecimal) exact).scale() == 0) {
        return BigDecimal.ONE;
      } else {
        return BigDecimal.ONE.setScale(1, NumberUtils.ROUNDING_MODE);
      }
    }
    return 1L;
  }
}
