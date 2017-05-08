package core.procedures.functional;

import core.procedures.AFn;

public final class VoidProc extends AFn {

  static final VoidProc VOID = new VoidProc();

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
    return core.scm.Void.VOID;
  }
}
