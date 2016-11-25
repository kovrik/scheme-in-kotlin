package core.procedures.math;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMBigRational;
import core.utils.NumberUtils;

import java.math.BigDecimal;
import java.math.MathContext;

public class Round extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "round";
  }

  @Override
  public Number invoke(Object... args) {
    if (args != null && args.length == 1) {
      if (args[0] instanceof Long) {
        return (Long)args[0];
      } else if (args[0] instanceof Double) {
        return Math.rint((Double)args[0]);
      } else if (args[0] instanceof BigDecimal) {
        BigDecimal number = (BigDecimal) args[0];
        if (number.scale() == 0) {
          return number.round(MathContext.UNLIMITED);
        } else {
          return number.round(NumberUtils.DEFAULT_CONTEXT);
        }
      } else if (args[0] instanceof SCMBigRational) {
        return ((SCMBigRational)args[0]).round();
      }
      throw new WrongTypeException("Number", args[0]);
    }
    throw new ArityException(args.length, 1, getName());
  }
}
