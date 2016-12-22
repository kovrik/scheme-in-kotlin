package core.procedures.system;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMError;

@FnArgs(minArgs = 1, maxArgs = 1)
public class ErrorProc extends AFn {

  @Override
  public String getName() {
    return "error";
  }

  @Override
  public Object apply(Object... args) {
    throw new SCMError(args[0].toString());
  }
}
