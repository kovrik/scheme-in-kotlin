package core.exceptions;

import core.scm.ISCMClass;
import core.scm.SCMClass;

public class ArityException extends IllegalArgumentException implements ISCMClass {

  public ArityException(int actual, String name) {
    this(actual, name, null);
  }

  public ArityException(int actual, int expected, String name, boolean variadic) {
    this(actual, expected, name, null, variadic);
  }

  public ArityException(int actual, String name, Throwable cause) {
    super("Wrong number of arguments (" + actual + ") passed to: " + (name.isEmpty() ? "#<procedure>" : name), cause);
  }

  public ArityException(int actual, int expected, String name, Throwable cause, boolean variadic) {
    super("Wrong number of arguments (actual: " + actual +
          ", expected: " + (variadic ? "at least " : "") + expected +
          ") passed to: " + (name.isEmpty() ? "#<procedure>" : name), cause);
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.ERROR;
  }
}
