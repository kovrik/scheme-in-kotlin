package core.procedures.system;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

public class ClassProc extends AFn {

  public ClassProc() {
    super(new FnArgsBuilder().min(1).max(1).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "class";
  }

  @Override
  public Class apply1(Object arg) {
    return arg == null ? null : arg.getClass();
  }
}
