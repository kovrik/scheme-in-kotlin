package core.procedures.math;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;

import java.math.BigDecimal;
import java.math.MathContext;

public class Round extends AFn {

  @Override
  public Number invoke(Object... args) {
    if (args != null && args.length == 1) {
      if (args[0] instanceof Long) {
        return (Long)args[0];
      } else if (args[0] instanceof Double) {
        return Math.rint((Double)args[0]);
      } else if (args[0] instanceof BigDecimal) {
        return ((BigDecimal)args[0]).round(MathContext.DECIMAL32);
      }
      throw new WrongTypeException("Number", args[0]);
    }
    throw new ArityException(args.length, 1, "round");
  }
}
