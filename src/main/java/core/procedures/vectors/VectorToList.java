package core.procedures.vectors;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMCons;
import core.scm.SCMMutableVector;
import core.scm.SCMVector;

public final class VectorToList extends AFn {

  public VectorToList() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{SCMVector.class}).build());
  }

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
