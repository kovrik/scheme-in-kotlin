package core.procedures.sets;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.HashMap;
import java.util.Map;

public final class MapInvert extends AFn {

  public MapInvert() {
    super(new FnArgsBuilder().minArgs(1).mandatoryArgsTypes(new Class[] {Map.class}));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "map-invert";
  }

  @Override
  public Map<Object, Object> apply(Object... args) {
    Map<Object, Object> result = new HashMap<>();
    for (Map.Entry<Object, Object> entry : ((Map<Object, Object>)args[0]).entrySet()) {
      result.put(entry.getValue(), entry.getKey());
    }
    return result;
  }
}
