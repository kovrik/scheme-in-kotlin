package core.exceptions;

import core.scm.ISCMClass;
import core.scm.SCMClass;
import core.scm.SCMDelay;
import core.writer.Writer;

public class ReentrantDelayException extends RuntimeException implements ISCMClass {

  public ReentrantDelayException(SCMDelay delay) {
    super(String.format("Re-entrant delay: %s", Writer.write(delay)), null);
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.ERROR;
  }
}
