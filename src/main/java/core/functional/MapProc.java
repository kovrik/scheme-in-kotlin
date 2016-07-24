package core.functional;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMCons;

import java.util.List;
import java.util.concurrent.ExecutionException;

public class MapProc extends AFn {

  @Override
  public List invoke(Object... args) throws ExecutionException, InterruptedException {
    if (args != null && args.length == 2) {
      if (!(args[0] instanceof AFn)) {
        throw new WrongTypeException("Procedure", args[0]);
      }
      AFn proc = (AFn)args[0];

      if (!(args[1] instanceof List)) {
        throw new WrongTypeException("List", args[1]);
      }
      List list = (List)args[1];
      SCMCons result = SCMCons.list();
      if (list.isEmpty()) {
        return result;
      }
      for (Object o : list) {
        // FIXME
        Object e = proc.invoke(o);
        result.add(e);
      }
    }
    throw new ArityException(args.length, 2, "map");
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
