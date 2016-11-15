package core.procedures.lists;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;

import java.util.List;

public class Length extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "length";
  }

  @Override
  public Long invoke(Object... args) {
    if (args != null && args.length == 1) {
      if (args[0] instanceof List) {
        return (long)((List)args[0]).size();
      }
      throw new WrongTypeException("List", args[0]);
    }
    throw new ArityException(args.length, 1, getName());
  }
}
