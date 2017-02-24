package core.procedures.system;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

public final class HashCode extends AFn {

  public HashCode() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "hashcode";
  }

  @Override
  public Object apply1(Object arg) {
    return arg.hashCode();
  }
}
