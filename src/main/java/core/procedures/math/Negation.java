package core.procedures.math;

import core.procedures.FnArgsBuilder;
import core.scm.SCMBoolean;
import core.procedures.AFn;

public final class Negation extends AFn {

  public Negation() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1));
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
    return !SCMBoolean.toBoolean(arg);
  }
}
