package core.procedures.generic;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.Cons;
import core.scm.MutableVector;
import core.scm.Vector;

import java.util.*;

public final class Empty extends AFn {

  public Empty() {
    super(new FnArgsBuilder().min(1).max(1).build());
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
      return Cons.list();
    }
    if (arg instanceof Set) {
      return new HashSet<>();
    }
    if (arg instanceof Vector) {
      return new MutableVector();
    }
    if (arg instanceof Map) {
      return new HashMap<>();
    }
    return null;
  }
}
