package core.procedures.hashmaps;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMVoid;

import java.util.HashMap;
import java.util.Map;

public final class Merge extends AFn {

  public Merge() {
    super(new FnArgsBuilder().min(0).rest(Map.class).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "merge";
  }

  @Override
  public Object apply(Object... args) {
    if (args.length == 0) {
      return SCMVoid.VOID;
    }
    Map<Object, Object> result = new HashMap<>();
    for (Object m : args) {
      result.putAll((Map)m);
    }
    return result;
  }
}
