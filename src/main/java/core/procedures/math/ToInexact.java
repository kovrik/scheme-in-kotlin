package core.procedures.math;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
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
      return NumberUtils.toInexact(args[0]);
    }
    throw new ArityException(args.length, 1, getName());
  }
}
