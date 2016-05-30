package core.procedures.delayed;

import core.scm.SCMBoolean;
import core.exceptions.ArityException;
import core.procedures.AFn;

import java.util.concurrent.ExecutionException;

@Deprecated
public class IsAPromise extends AFn {

  @Override
  public SCMBoolean invoke(Object... args) throws ExecutionException, InterruptedException {
    if (args.length < 1) {
      throw new ArityException(0, "promise?");
    }
    for (Object arg : args) {
      if (!(arg instanceof SCMPromise)) {
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
