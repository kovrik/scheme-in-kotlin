package core.procedures.hashmaps;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.IMapEntry;

public final class Val extends AFn {

  public Val() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{IMapEntry.class}));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "val";
  }

  @Override
  public Object apply1(Object arg) {
    return ((IMapEntry)arg).val();
  }
}
