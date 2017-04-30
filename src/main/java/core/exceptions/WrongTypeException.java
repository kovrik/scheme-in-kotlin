package core.exceptions;

import core.writer.Writer;

public class WrongTypeException extends IllegalArgumentException implements ISCMException {

  public WrongTypeException(String name, String expected, Object given) {
    super((name.isEmpty() ? "#<procedure>" : name) + ": type mismatch; " + "(" +
           "expected: " + expected + ", given: " + Writer.write(given) + ")", null);
  }

  @Override
  public synchronized Throwable fillInStackTrace() {
    return null;
  }
}
