package core.exceptions;

import core.scm.ISCMClass;

public class ReentrantContinuationException extends RuntimeException implements ISCMClass, ISCMException {

  public ReentrantContinuationException() {
    super("Re-entrant continuation: implementation restriction: continuation can only be used once");
  }

  @Override
  public synchronized Throwable fillInStackTrace() {
    return null;
  }
}
