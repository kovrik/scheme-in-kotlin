package core.procedures.math;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.BigRational;
import core.utils.Utils;

import java.math.BigDecimal;

public final class Numerator extends AFn {

  public Numerator() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{BigRational.class}).build());
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
    if (exact instanceof BigRational) {
      BigDecimal result = new BigDecimal(((BigRational) exact).getNumerator());
      if (!isExact) {
        return result.setScale(1, Utils.ROUNDING_MODE);
      }
      return result;
    }
    return exact;
  }
}
