package core.procedures.lists;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.scm.SCMList;

import java.util.List;

public class Length extends AFn {

  @Override
  public Long invoke(Object... args) {
    if (args != null && args.length == 1) {
      if (args[0] instanceof List) {
        return (long)((List)args[0]).size();
      }
      throw new IllegalArgumentException(
          String.format("Wrong type argument to `length`! Expected: %s, Actual: %s", SCMList.class, args[0].getClass()));
    }
    throw new ArityException(args.length, "length");
  }

  @Override
  public Object call() throws Exception {
    return invoke();
  }

  @Override
  public void run() {
    invoke();
  }
}
