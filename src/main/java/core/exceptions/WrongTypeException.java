package core.exceptions;

import core.scm.ISCMClass;
import core.writer.Writer;

public class WrongTypeException extends IllegalArgumentException implements ISCMClass, ISCMException {

  public WrongTypeException(String name, String expected, Object given) {
    super((name.isEmpty() ? "#<procedure>" : name) + ": type mismatch; " + "(" +
           "expected: " + expected + ", given: " + Writer.write(given) + ")", null);
  }

  @Override
  public synchronized Throwable fillInStackTrace() {
    return null;
  }
}
