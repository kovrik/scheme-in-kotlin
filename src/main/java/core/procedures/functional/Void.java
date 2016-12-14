package core.procedures.functional;

import core.procedures.AFn;
import core.scm.FnArgs;

import static core.scm.SCMUnspecified.UNSPECIFIED;

@FnArgs(isVariadic = true)
public class Void extends AFn {

  static final Void INSTANCE = new Void();

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "void";
  }

  @Override
  public Object apply(Object... args) {
    return UNSPECIFIED;
  }
}
