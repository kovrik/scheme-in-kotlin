package core.procedures.system;

import core.procedures.AFn;
import core.scm.FnArgs;

@FnArgs(minArgs = 1, maxArgs = 1)
public class HashCode extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "hashcode";
  }

  @Override
  public Object apply(Object... args) {
    return args[0].hashCode();
  }
}
