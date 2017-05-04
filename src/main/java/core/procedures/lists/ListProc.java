package core.procedures.lists;

import core.procedures.AFn;
import core.scm.Cons;

import java.util.List;

public final class ListProc extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "list";
  }

  @Override
  public List<Object> apply(Object... args) {
    return Cons.list(args);
  }
}
