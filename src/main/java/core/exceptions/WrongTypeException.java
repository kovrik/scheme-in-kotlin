package core.exceptions;

import core.scm.Type;
import core.writer.Writer;

public class WrongTypeException extends IllegalArgumentException {

  public WrongTypeException(String message) {
    super(message);
  }

  public WrongTypeException(String name, String expected, Object given) {
    super((name.isEmpty() ? "#<procedure>" : name) + ": type mismatch; " + "(" +
           "expected: " + expected + ", given: " + Writer.write(given) + ")", null);
  }

  public WrongTypeException(String name, Class expected, Object given) {
    this(name, Type.nameOf(expected), given);
  }

  @Override
  public synchronized Throwable fillInStackTrace() {
    return null;
  }
}
