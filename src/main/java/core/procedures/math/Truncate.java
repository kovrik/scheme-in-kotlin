package core.procedures.math;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;

import java.math.BigDecimal;

public class Truncate extends AFn {

  @Override
  public Number invoke(Object... args) {
    if (args != null && args.length == 1) {
      if (args[0] instanceof Long) {
        return (Long)args[0];
      } else if (args[0] instanceof Double) {
        Double arg = (Double) args[0];
        if (arg < 0) {
          return Math.ceil(arg);
        } else {
          return Math.floor(arg);
        }
      } else if (args[0] instanceof BigDecimal) {
        BigDecimal arg = (BigDecimal)args[0];
        if (arg.compareTo(BigDecimal.ZERO) < 0) {
          return arg.setScale(0, BigDecimal.ROUND_UP);
        } else {
          return arg.setScale(0, BigDecimal.ROUND_DOWN);
        }
      }
      throw new WrongTypeException("Number", args[0]);
    }
    throw new ArityException(args.length, 1, "truncate");
  }
}
