package core.procedures.meta;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.IMeta;

import java.util.Map;

public final class MetaProc extends AFn {

  public MetaProc() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[] {IMeta.class}).build());
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
