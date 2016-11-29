package core.procedures.math;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.predicates.IsNumber;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;

public class ToInexact extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "exact->inexact";
  }

  @Override
  public Number invoke(Object... args) {
    if (args != null && args.length == 1) {
      return toInexact(args[0]);
    }
    throw new ArityException(args.length, 1, getName());
  }

  public static Number toInexact(Object o) {
    if (!IsNumber.isNumber(o)) {
      throw new WrongTypeException("Number", o);
    }
    if (o instanceof SCMBigRational) {
      return ((SCMBigRational)o).toBigDecimalInexact();
    }
    if (o instanceof BigDecimal) {
      int scale = Math.max(1, ((BigDecimal)o).scale());
      return ((BigDecimal)o).setScale(scale, NumberUtils.ROUNDING_MODE);
    }
    return ((Number)o).doubleValue();
  }
}
