package core.procedures.system;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

public class HashCode extends AFn {

  public HashCode() {
    super(new FnArgsBuilder().min(1).max(1).build());
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
