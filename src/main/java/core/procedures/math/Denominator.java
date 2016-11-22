package core.procedures.math;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.utils.NumberUtils;

public class Denominator extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "denominator";
  }

  @Override
  public Number invoke(Object... args) {
    if (args != null && args.length == 1) {
      return NumberUtils.denominator(args[0]);
    }
    throw new ArityException(1, getName());
  }
}
