package core.exceptions;

public class SCMFileNotFoundException extends RuntimeException implements IException {

  public SCMFileNotFoundException(String filename) {
    super("Cannot open file: " + filename);
  }

  @Override
  public synchronized Throwable fillInStackTrace() {
    return null;
  }
}
