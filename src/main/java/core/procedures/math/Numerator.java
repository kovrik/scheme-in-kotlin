package core.procedures.math;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;

@FnArgs(args = {Number.class}) // Rational?
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
  public Number invoke(Object... args) {
    return numerator(args[0]);
  }

  public static Number numerator(Object o) {
    if (!NumberUtils.isRational(o)) {
      throw new WrongTypeException("Rational", o);
    }
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
