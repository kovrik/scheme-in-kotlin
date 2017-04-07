package core.procedures.hashmaps;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMCons;

import java.util.Map;

public final class Keys extends AFn {

  public Keys() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{Map.class}));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "keys";
  }

  @Override
  public Object apply1(Object arg) {
    return SCMCons.list(((Map)arg).keySet());
  }
}
