package core.procedures.meta;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.IMeta;

import java.util.Map;

public final class MetaProc extends AFn {

  public MetaProc() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[] {IMeta.class}));
  }

  @Override
  public boolean isPure() {
    return false;
  }

  @Override
  public String getName() {
    return "meta";
  }

  @Override
  public Map apply1(Object arg) {
    return ((IMeta)arg).meta();
  }
}
