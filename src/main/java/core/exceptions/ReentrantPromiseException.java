package core.exceptions;

import core.scm.ISCMClass;
import core.scm.SCMClass;
import core.scm.SCMPromise;
import core.writer.Writer;

public class ReentrantPromiseException extends RuntimeException implements ISCMClass {

  public ReentrantPromiseException(SCMPromise promise) {
    super(String.format("Re-entrant promise: %s", Writer.write(promise)), null);
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.ERROR;
  }
}
