package core.procedures.hashmaps;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMCons;

import java.util.Map;

public final class Values extends AFn {

  public Values() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{Map.class}));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "values";
  }

  @Override
  public Object apply1(Object arg) {
    return SCMCons.list(((Map)arg).values());
  }
}
