package core.procedures.vectors;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMCons;
import core.scm.SCMMutableVector;

import java.util.List;

@FnArgs(args = {SCMCons.SCMProperList.class})
public class ListToVector extends AFn {

  @Override
  public String getName() {
    return "list->vector";
  }

  @Override
  public SCMMutableVector invoke(Object... args) {
    return listToVector(args[0]);
  }

  public static SCMMutableVector listToVector(Object arg) {
    return new SCMMutableVector(((List) arg).toArray());
  }
}
