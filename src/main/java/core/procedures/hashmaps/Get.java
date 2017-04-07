package core.procedures.hashmaps;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.Map;

public final class Get extends AFn {

  public Get() {
    super(new FnArgsBuilder().minArgs(2).maxArgs(3).mandatoryArgsTypes(new Class[]{Map.class, Object.class, Object.class}));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "get";
  }

  @Override
  public Object apply(Object... args) {
    if (args.length == 3) {
      return ((Map) args[0]).getOrDefault(args[1], args[2]);
    } else {
      return ((Map) args[0]).get(args[1]);
    }
  }
}
