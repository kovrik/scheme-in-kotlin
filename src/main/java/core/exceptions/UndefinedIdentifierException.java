package core.exceptions;

public class UndefinedIdentifierException extends RuntimeException implements ISCMException {

  public UndefinedIdentifierException(String identifier) {
    super("unable to resolve symbol: " + identifier);
  }

  @Override
  public synchronized Throwable fillInStackTrace() {
    return null;
  }
}