package core.procedures.math;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;

import java.math.BigDecimal;

public class Abs extends AFn {

  @Override
  public String getName() {
    return "abs";
  }

  @Override
  public Number invoke(Object... args) {
    if (args != null && args.length == 1) {
      if (args[0] instanceof Long) {
        return Math.abs((Long)args[0]);
      } else if (args[0] instanceof Double) {
        return Math.abs((Double) args[0]);
      } else if (args[0] instanceof BigDecimal) {
        return ((BigDecimal)args[0]).abs();
      }
      throw new WrongTypeException("Number", args[0]);
    }
    throw new ArityException(args.length, 1, getName());
  }
}
