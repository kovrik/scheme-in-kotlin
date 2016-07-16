package core.procedures.lists;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.scm.SCMBoolean;

import java.util.List;

public class Member extends AFn {

  @Override
  public Object invoke(Object... args) {
    if (args != null && args.length == 2) {
      if (!(args[1] instanceof List)) {
        throw new IllegalArgumentException(
            String.format("Wrong type argument to `member`! Expected: List, Actual: %s", args[1]));
      }
      List list = (List)args[1];
      int i = list.indexOf(args[0]);
      if (i == -1) {
        return SCMBoolean.FALSE;
      }
      if (i == 0) {
        return list;
      }
      return list.subList(i, list.size());
    }
    throw new ArityException(args.length, 2, "member");
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
