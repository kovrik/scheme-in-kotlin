package core.exceptions;

import core.scm.ISCMClass;
import core.scm.SCMClass;

public class IllegalSyntaxException extends RuntimeException implements ISCMClass {

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
  public SCMClass getSCMClass() {
    return SCMClass.ERROR;
  }
}
