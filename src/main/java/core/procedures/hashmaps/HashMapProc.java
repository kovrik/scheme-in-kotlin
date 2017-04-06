package core.procedures.hashmaps;

import core.procedures.AFn;

import java.util.HashMap;
import java.util.Map;

public final class HashMapProc extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "hash-map";
  }

  @Override
  public Map<Object, Object> apply(Object... args) {
    if (args.length % 2 != 0) {
      throw new IllegalArgumentException("hash-map: no value supplied for key: " + args[args.length - 1]);
    }
    Map<Object, Object> result = new HashMap<>();
    for (int i = 0; i < args.length; i = i + 2) {
      result.put(args[i], args[i + 1]);
    }
    return result;
  }
}
