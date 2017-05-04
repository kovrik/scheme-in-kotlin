package core.procedures.math;

import core.procedures.FnArgsBuilder;
import core.procedures.AFn;
import core.utils.Utils;

public final class Negation extends AFn {

  public Negation() {
    super(new FnArgsBuilder().min(1).max(1).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "not";
  }

  @Override
  public Boolean apply1(Object arg) {
    return !Utils.toBoolean(arg);
  }
}
