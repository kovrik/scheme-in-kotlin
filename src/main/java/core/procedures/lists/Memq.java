package core.procedures.lists;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.procedures.equivalence.Eq;
import core.scm.SCMBoolean;

import java.util.List;

public class Memq extends AFn {

  @Override
  public Object invoke(Object... args) {
    if (args != null && args.length == 2) {
      if (!(args[1] instanceof List)) {
        throw new IllegalArgumentException(
            String.format("Wrong type argument to `memq`! Expected: List, Actual: %s", args[1]));
      }
      List list = (List)args[1];
      int i = -1;
      for (int n = 0, listSize = list.size(); n < listSize; n++) {
        if (Eq.eq(args[0], list.get(n))) {
          i = n;
          break;
        }
      }
      if (i == -1) {
        return SCMBoolean.FALSE;
      }
      if (i == 0) {
        return list;
      }
      return list.subList(i, list.size());
    }
    throw new ArityException(args.length, 2, "memq");
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
