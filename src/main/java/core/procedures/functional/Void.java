package core.procedures.functional;

import core.procedures.AFn;

import static core.scm.SCMConstant.UNSPECIFIED;

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
    return UNSPECIFIED;
  }
}
