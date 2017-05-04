package core.procedures.vectors;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMClass;
import core.scm.SCMMutableVector;

import java.util.List;

public final class ListToVector extends AFn {

  public ListToVector() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{SCMClass.SCMProperList.class}).build());
  }

  @Override
  public String getName() {
    return "list->vector";
  }

  @Override
  public SCMMutableVector apply1(Object arg) {
    return listToVector(arg);
  }

  public static SCMMutableVector listToVector(Object arg) {
    return new SCMMutableVector(((List) arg).toArray());
  }
}
