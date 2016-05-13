package main.core.procedures.vectors;

import main.core.ast.SCMBoolean;
import main.core.ast.SCMVector;
import main.core.exceptions.ArityException;
import main.core.procedures.AFn;

import java.util.concurrent.ExecutionException;

public class IsAVector extends AFn {

  @Override
  public SCMBoolean invoke(Object... args) throws ExecutionException, InterruptedException {
    if (args.length < 1) {
      throw new ArityException(0, "vector?");
    }
    for (Object arg : args) {
      if (!(arg instanceof SCMVector)) {
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
