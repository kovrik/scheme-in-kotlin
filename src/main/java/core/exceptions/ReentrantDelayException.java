package core.exceptions;

import core.scm.Delay;
import core.writer.Writer;

public class ReentrantDelayException extends RuntimeException implements IException {

  public ReentrantDelayException(Delay delay) {
    super(String.format("Re-entrant delay: %s", Writer.write(delay)), null);
  }

  @Override
  public synchronized Throwable fillInStackTrace() {
    return null;
  }
}
