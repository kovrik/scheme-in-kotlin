package core.exceptions;

public class ArityException extends IllegalArgumentException {

  public ArityException(int actual, String name) {
    this(actual, name, null);
  }

  public ArityException(int actual, int expected, String name) {
    this(actual, expected, name, null);
  }

  public ArityException(int actual, String name, Throwable cause) {
    super("Wrong number of arguments (" + actual + ") passed to: " + name, cause);
  }

  public ArityException(int actual, int expected, String name, Throwable cause) {
    super("Wrong number of arguments (actual: " + actual + ", expected: " + expected + ") passed to: " + name, cause);
  }
}
