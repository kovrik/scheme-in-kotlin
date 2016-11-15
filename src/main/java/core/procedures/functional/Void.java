package core.procedures.functional;

import core.procedures.AFn;

import static core.scm.SCMUnspecified.UNSPECIFIED;

public class Void extends AFn {

  public static final Void INSTANCE = new Void();

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "void";
  }

  @Override
  public Object invoke(Object... args) {
    return UNSPECIFIED;
  }
}
