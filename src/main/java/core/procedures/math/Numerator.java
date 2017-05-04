package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;

public final class Numerator extends AFn {

  public Numerator() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{SCMBigRational.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "numerator";
  }

  @Override
  public Number apply1(Object arg) {
    return numerator(arg);
  }

  private Number numerator(Object o) {
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
