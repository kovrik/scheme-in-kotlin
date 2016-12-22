package core.procedures.vectors;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMCons;
import core.scm.SCMMutableVector;
import core.scm.SCMVector;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {SCMVector.class})
public class VectorToList extends AFn {

  @Override
  public String getName() {
    return "vector->list";
  }

  @Override
  public SCMCons apply(Object... args) {
    return vectorToList((SCMMutableVector)args[0]);
  }

  public static SCMCons vectorToList(SCMMutableVector v) {
    return SCMCons.list((v).getArray());
  }
}
