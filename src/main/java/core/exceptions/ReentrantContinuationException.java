package core.exceptions;

public class ReentrantContinuationException extends RuntimeException implements ISCMException {

  public ReentrantContinuationException() {
    super("Re-entrant continuation: implementation restriction: continuation can only be used once");
  }

  @Override
  public synchronized Throwable fillInStackTrace() {
    return null;
  }
}
