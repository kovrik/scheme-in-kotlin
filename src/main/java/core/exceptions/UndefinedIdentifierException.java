package core.exceptions;

import core.scm.ISCMClass;

public class UndefinedIdentifierException extends RuntimeException implements ISCMClass, ISCMException {

  public UndefinedIdentifierException(String identifier) {
    super("unable to resolve symbol: " + identifier);
  }

  @Override
  public synchronized Throwable fillInStackTrace() {
    return null;
  }
}
