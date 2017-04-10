package core.procedures.generic;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMCons;
import core.scm.SCMMutableVector;
import core.scm.SCMVector;

import java.util.*;

public final class Empty extends AFn {

  public Empty() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "empty";
  }

  @Override
  public Object apply1(Object arg) {
    if (arg instanceof List) {
      return SCMCons.list();
    }
    if (arg instanceof Set) {
      return new HashSet<>();
    }
    if (arg instanceof SCMVector) {
      return new SCMMutableVector();
    }
    if (arg instanceof Map) {
      return new HashMap<>();
    }
    return null;
  }
}
