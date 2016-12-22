package core.procedures.lists;

import core.procedures.AFn;
import core.scm.SCMCons;

import java.util.List;

public class ListProc extends AFn {

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
    return SCMCons.list(args);
  }
}
