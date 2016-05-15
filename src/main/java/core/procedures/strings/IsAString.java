package core.procedures.strings;

import core.scm.SCMBoolean;
import core.exceptions.ArityException;
import core.procedures.AFn;

import java.util.concurrent.ExecutionException;

@Deprecated
public class IsAString extends AFn {

  @Override
  public SCMBoolean invoke(Object... args) throws ExecutionException, InterruptedException {
    if (args.length < 1) {
      throw new ArityException(0, "string?");
    }
    for (Object arg : args) {
      if (!(arg instanceof String)) {
        return SCMBoolean.FALSE;
      }
    }
    return SCMBoolean.TRUE;
  }

  @Override
  public SCMBoolean call() throws Exception {
    return SCMBoolean.FALSE;
  }
}
