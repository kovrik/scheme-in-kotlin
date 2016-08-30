package core.procedures.math;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;

import java.math.BigDecimal;

public class Expt extends AFn {

  @Override
  public String getName() {
    return "expt";
  }

  @Override
  public Number invoke(Object... args) {
    if (args != null && args.length == 2) {
      if (!(args[0] instanceof Number)) {
        throw new WrongTypeException("Number", args[0]);
      }
      if (!(args[1] instanceof Number)) {
        throw new WrongTypeException("Number", args[1]);
      }
      return invoke((Number)args[0], (Number)args[1]);
    }
    throw new ArityException(args.length, 2, getName());
  }

  // FIXME Fractional power for BigDecimals?
  public Number invoke(Number first, Number second) {
    if ((first instanceof BigDecimal) || (second instanceof BigDecimal)) {
      return new BigDecimal(first.toString()).pow(second.intValue());
    }
    double result = Math.pow(first.doubleValue(), second.doubleValue());
    if (Double.isInfinite(result)) {
      return new BigDecimal(first.toString()).pow(second.intValue());
    }
    return result;
  }
}
