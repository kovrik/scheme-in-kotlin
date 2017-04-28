package core.exceptions;

public class SCMFileNotFoundException extends RuntimeException implements ISCMException {

  public SCMFileNotFoundException(String filename) {
    super("Cannot open file: " + filename);
  }

  @Override
  public synchronized Throwable fillInStackTrace() {
    return null;
  }
}
