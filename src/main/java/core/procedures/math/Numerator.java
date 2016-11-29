package core.procedures.math;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.predicates.IsExact;
import core.procedures.predicates.IsRational;
import core.scm.SCMBigRational;

import java.math.BigDecimal;

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
    if (args != null && args.length == 1) {
      return numerator(args[0]);
    }
    throw new ArityException(1, getName());
  }

  public static Number numerator(Object o) {
    if (!IsRational.isRational(o)) {
      throw new WrongTypeException("Rational", o);
    }
    boolean isExact = IsExact.isExact(o);
    Number exact;
    if (isExact) {
      exact = (Number)o;
    } else {
      exact = ToExact.toExact(o);
    }
    if (exact instanceof SCMBigRational) {
      BigDecimal result = new BigDecimal(((SCMBigRational) exact).getNumerator());
      if (!isExact) {
        return result.setScale(1);
      }
      return result;
    }
    return exact;
  }
}
