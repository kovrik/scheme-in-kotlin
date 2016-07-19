package core.exceptions;

import core.writer.Writer;

public class WrongTypeException extends IllegalArgumentException {

  public WrongTypeException(String expected, Object actual) {
    super(String.format("Wrong argument type. Expected: %s, actual: %s", expected, Writer.write(actual)), null);
  }

  public WrongTypeException(String expected, Object actual, Throwable cause) {
    super(String.format("Wrong argument type. Expected: %s, actual: %s", expected, Writer.write(actual)), cause);
  }
}
