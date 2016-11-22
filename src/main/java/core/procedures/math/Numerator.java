package core.procedures.math;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.utils.NumberUtils;

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
      return NumberUtils.numerator(args[0]);
    }
    throw new ArityException(1, getName());
  }
}
