package core.exceptions;

public class SCMFileNotFoundException extends RuntimeException {

  public SCMFileNotFoundException(String filename) {
    super("Cannot open file: " + filename);
  }

  @Override
  public synchronized Throwable fillInStackTrace() {
    return null;
  }
}
