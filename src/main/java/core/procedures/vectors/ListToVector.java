package core.procedures.vectors;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMClass;
import core.scm.SCMMutableVector;

import java.util.List;

public final class ListToVector extends AFn {

  public ListToVector() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{SCMClass.SCMProperList.class}));
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
