package core.procedures.system;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

public class Num extends AFn {

  public Num() {
    super(new FnArgsBuilder().min(1).max(1).build());
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
