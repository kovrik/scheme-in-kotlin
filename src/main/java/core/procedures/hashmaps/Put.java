package core.procedures.hashmaps;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.HashMap;
import java.util.Map;

// TODO Rename to Assoc?
public final class Put extends AFn {

  public Put() {
    super(new FnArgsBuilder().minArgs(3).mandatoryArgsTypes(new Class[]{Map.class, Object.class, Object.class}));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "put";
  }

  @Override
  public Map<Object, Object> apply(Object... args) {
    if (args.length % 2 != 1) {
      throw new IllegalArgumentException(getName() + ": no value supplied for key: " + args[args.length - 1]);
    }
    Map<Object, Object> map = new HashMap((Map)args[0]);
    for (int i = 1; i < args.length; i = i + 2) {
      map.put(args[i], args[i + 1]);
    }
    return map;
  }
}
