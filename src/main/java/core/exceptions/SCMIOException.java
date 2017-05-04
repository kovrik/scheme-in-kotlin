package core.exceptions;

public class SCMIOException extends RuntimeException implements IException {

  public SCMIOException(Throwable e) {
    super(e);
  }

  @Override
  public synchronized Throwable fillInStackTrace() {
    return null;
  }
}
