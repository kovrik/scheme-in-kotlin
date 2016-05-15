package core.procedures.characters;

import core.scm.SCMBoolean;
import core.exceptions.ArityException;
import core.procedures.AFn;

import java.util.concurrent.ExecutionException;

public class IsAChar extends AFn {

  @Override
  public SCMBoolean invoke(Object... args) throws ExecutionException, InterruptedException {
    if (args.length < 1) {
      throw new ArityException(0, "char?");
    }
    for (Object arg : args) {
      if (!(arg instanceof Character)) {
        return SCMBoolean.FALSE;
      }
    }
    return SCMBoolean.TRUE;
  }

  @Override
  public Object call() throws Exception {
    return SCMBoolean.FALSE;
  }
}
