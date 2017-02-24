package core.procedures.system;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMError;

public final class ErrorProc extends AFn {

  public ErrorProc() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1));
  }

  @Override
  public String getName() {
    return "error";
  }

  @Override
  public Object apply1(Object arg) {
    throw new SCMError(arg.toString());
  }
}
