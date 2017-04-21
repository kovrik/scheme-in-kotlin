package core.procedures.system;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMNil;

public class ClassProc extends AFn {

  public ClassProc() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1));
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
    return (arg == null || arg == SCMNil.NIL) ? null : arg.getClass();
  }
}
