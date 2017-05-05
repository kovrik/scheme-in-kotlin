package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.BigRatio;
import core.utils.Utils;

import java.math.BigDecimal;

public final class Numerator extends AFn {

  public Numerator() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{BigRatio.class}).build());
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
    boolean isExact = Utils.isExact(o);
    Number exact;
    if (isExact) {
      exact = (Number)o;
    } else {
      exact = ToExact.toExact(o);
    }
    if (exact instanceof BigRatio) {
      if (!isExact) {
        BigDecimal result = new BigDecimal(((BigRatio) exact).getNumerator());
        return result.setScale(1, Utils.ROUNDING_MODE);
      }
      return ((BigRatio) exact).getNumerator();
    }
    return exact;
  }
}
