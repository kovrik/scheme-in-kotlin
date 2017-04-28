package core.exceptions;

import core.scm.ISCMClass;
import core.scm.SCMDelay;
import core.writer.Writer;

public class ReentrantDelayException extends RuntimeException implements ISCMClass, ISCMException {

  public ReentrantDelayException(SCMDelay delay) {
    super(String.format("Re-entrant delay: %s", Writer.write(delay)), null);
  }

  @Override
  public synchronized Throwable fillInStackTrace() {
    return null;
  }
}
