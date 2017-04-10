package core.procedures.hashmaps;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.HashMap;
import java.util.Map;

public final class Merge extends AFn {

  public Merge() {
    super(new FnArgsBuilder().minArgs(0).restArgsType(Map.class));
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
      return null;
    }
    Map<Object, Object> result = new HashMap<>();
    for (Object m : args) {
      result.putAll((Map)m);
    }
    return result;
  }
}
