package main.core.procedures.delayed;

import main.core.ast.SCMBoolean;
import main.core.exceptions.ArityException;
import main.core.procedures.AFn;

import java.util.concurrent.ExecutionException;

public class IsAPromise extends AFn {

  @Override
  public SCMBoolean invoke(Object... args) throws ExecutionException, InterruptedException {
    if (args.length < 1) {
      throw new ArityException(0, "promise?");
    }
    for (Object arg : args) {
      if (!(arg instanceof Promise)) {
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
