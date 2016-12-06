package core.procedures.system;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMError;

@FnArgs(args = {Object.class})
public class ErrorProc extends AFn {

  @Override
  public String getName() {
    return "error";
  }

  @Override
  public Object invoke(Object... args) {
    throw new SCMError(args[0].toString());
  }
}
