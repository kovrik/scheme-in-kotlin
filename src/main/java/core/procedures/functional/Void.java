package core.procedures.functional;

import core.procedures.AFn;
import core.scm.SCMVoid;

public final class Void extends AFn {

  static final Void VOID = new Void();

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
    return SCMVoid.VOID;
  }
}
