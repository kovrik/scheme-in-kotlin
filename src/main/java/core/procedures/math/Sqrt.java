package core.procedures.math;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;

import java.math.BigDecimal;

public class Sqrt extends AFn {

  @Override
  public Number invoke(Object... args) {
    if (args != null && args.length == 1) {
      if (args[0] instanceof Long) {
        return Math.sqrt((Long)args[0]);
      } else if (args[0] instanceof Double) {
        return Math.sqrt((Double) args[0]);
      } else if (args[0] instanceof BigDecimal) {
        return Double.POSITIVE_INFINITY;
      }
      throw new WrongTypeException("Number", args[0]);
    }
    throw new ArityException(args.length, 1, "sqrt");
  }
}
