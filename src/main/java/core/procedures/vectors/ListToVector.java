package core.procedures.vectors;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMVector;

import java.util.List;

public class ListToVector extends AFn {

  @Override
  public SCMVector invoke(Object... args) {
    if (args != null && args.length == 1) {
      if (args[0] instanceof List) {
        List list = (List) args[0];
        return new SCMVector(list.toArray());
      }
      throw new WrongTypeException("List", args[0]);
    }
    throw new ArityException(args.length, 1, "list->vector");
  }

  public Number zero() {
    throw new ArityException(0, 1, "list->vector");
  }

  public Number apply(Number first, Number second) {
    throw new ArityException(2, 1, "list->vector");
  }

  public Object apply(Object first, Object second) {
    throw new ArityException(2, 1, "list->vector");
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
