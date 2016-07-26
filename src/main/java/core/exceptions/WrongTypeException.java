package core.exceptions;

import core.scm.ISCMClass;
import core.scm.SCMClass;
import core.writer.Writer;

public class WrongTypeException extends IllegalArgumentException implements ISCMClass {

  public WrongTypeException(String expected, Object actual) {
    super(String.format("Wrong argument type. Expected: %s, actual: %s", expected, Writer.write(actual)), null);
  }

  public WrongTypeException(String expected, Object actual, Throwable cause) {
    super(String.format("Wrong argument type. Expected: %s, actual: %s", expected, Writer.write(actual)), cause);
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.ERROR;
  }
}
