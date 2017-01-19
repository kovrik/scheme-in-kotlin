package core.procedures.vectors;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMCons;
import core.scm.SCMMutableVector;
import core.scm.SCMVector;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {SCMVector.class})
public final class VectorToList extends AFn {

  @Override
  public String getName() {
    return "vector->list";
  }

  @Override
  public SCMCons apply1(Object arg) {
    return vectorToList((SCMMutableVector)arg);
  }

  public static SCMCons vectorToList(SCMMutableVector v) {
    return SCMCons.list((v).getArray());
  }
}
