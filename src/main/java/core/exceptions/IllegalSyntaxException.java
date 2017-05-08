package core.exceptions;

public class IllegalSyntaxException extends RuntimeException {

  public IllegalSyntaxException(String message) {
    super(message);
  }

  public static IllegalSyntaxException of(String syntax, Object expression, String description) {
    String message;
    if (description != null) {
      message = String.format("%s: bad syntax (%s) in form: %s", syntax, description, expression);
    } else {
      message = String.format("%s: bad syntax in form: %s", syntax, expression);
    }
    return new IllegalSyntaxException(message);
  }

  public static IllegalSyntaxException of(String syntax, Object expression) {
    return of(syntax, expression, null);
  }

  @Override
  public synchronized Throwable fillInStackTrace() {
    return null;
  }
}
