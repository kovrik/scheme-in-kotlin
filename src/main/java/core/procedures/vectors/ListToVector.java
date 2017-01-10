package core.procedures.vectors;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMClass;
import core.scm.SCMMutableVector;

import java.util.List;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {SCMClass.SCMProperList.class})
public class ListToVector extends AFn {

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
