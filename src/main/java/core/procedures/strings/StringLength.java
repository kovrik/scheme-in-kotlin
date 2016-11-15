package core.procedures.strings;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;

public class StringLength extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "string-length";
  }

  @Override
  public Long invoke(Object... args) {
    if (args != null && args.length == 1) {
      if (args[0] instanceof String) {
        return ((Integer)((String)args[0]).length()).longValue();
      }
      throw new WrongTypeException("String", args[0]);
    }
    throw new ArityException(args.length, 1, getName());
  }
}
