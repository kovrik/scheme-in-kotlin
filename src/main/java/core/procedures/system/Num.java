package core.procedures.system;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

public class Num extends AFn {

  public Num() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "num";
  }

  @Override
  public Number apply1(Object arg) {
    return ((Number)arg);
  }
}
