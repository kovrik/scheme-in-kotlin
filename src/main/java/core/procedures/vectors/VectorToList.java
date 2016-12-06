package core.procedures.vectors;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMCons;
import core.scm.SCMVector;

@FnArgs(args = {SCMVector.class})
public class VectorToList extends AFn {

  @Override
  public String getName() {
    return "vector->list";
  }

  @Override
  public SCMCons invoke(Object... args) {
    return SCMCons.list(((SCMVector) args[0]).getArray());
  }
}
