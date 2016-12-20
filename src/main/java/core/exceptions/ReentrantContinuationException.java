package core.exceptions;

import core.scm.ISCMClass;
import core.scm.SCMClass;

public class ReentrantContinuationException extends RuntimeException implements ISCMClass {

  public ReentrantContinuationException() {
    super("Re-entrant continuation: implementation restriction: continuation can only be used once");
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.ERROR;
  }
}
