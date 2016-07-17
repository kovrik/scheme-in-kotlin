package core.procedures.lists;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.procedures.equivalence.Eq;
import core.scm.SCMBoolean;
import core.scm.SCMCons;

import java.util.List;

public class Assq extends AFn {

  @Override
  public Object invoke(Object... args) {
    if (args != null && args.length == 2) {
      if (!(args[1] instanceof List)) {
        throw new IllegalArgumentException(
            String.format("Wrong type argument to `assq`! Expected: List, Actual: %s", args[1]));
      }
      Object obj = args[0];
      List list = (List)args[1];
      for (int n = 0, listSize = list.size(); n < listSize; n++) {

        Object pair = list.get(n);
        if (SCMCons.isPair(pair)) {
          if (Eq.eq(obj, ((SCMCons)pair).car())) {
            return pair;
          }
        } else {
          throw new IllegalArgumentException(
              String.format("Wrong type argument in position %s (expecting association list): %s", n, list));
        }
      }
      return SCMBoolean.FALSE;
    }
    throw new ArityException(args.length, 2, "assq");
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
