package core.procedures.math;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.utils.NumberUtils;

public class ToExact extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "inexact->exact";
  }

  @Override
  public Number invoke(Object... args) {
    if (args != null && args.length == 1) {
      return NumberUtils.toExact(args[0]);
    }
    throw new ArityException(args.length, 1, getName());
  }
}
