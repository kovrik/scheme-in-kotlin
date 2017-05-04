package core.procedures.vectors;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.Cons;
import core.scm.MutableVector;
import core.scm.Vector;

public final class VectorToList extends AFn {

  public VectorToList() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Vector.class}).build());
  }

  @Override
  public String getName() {
    return "vector->list";
  }

  @Override
  public Cons apply1(Object arg) {
    return vectorToList((MutableVector)arg);
  }

  public static Cons vectorToList(MutableVector v) {
    return Cons.list((v).getArray());
  }
}
