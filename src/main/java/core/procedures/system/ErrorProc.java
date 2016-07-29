package core.procedures.system;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.scm.SCMError;

public class ErrorProc extends AFn {

  @Override
  public String getName() {
    return "error";
  }

  @Override
  public Object invoke(Object... args) {
    if (args.length > 1) {
      throw new ArityException(args.length, 1, "error");
    }
    throw new SCMError(args[0].toString());
  }
}
