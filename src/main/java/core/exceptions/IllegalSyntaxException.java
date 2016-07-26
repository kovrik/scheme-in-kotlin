package core.exceptions;

import core.scm.ISCMClass;
import core.scm.SCMClass;

public class IllegalSyntaxException extends RuntimeException implements ISCMClass {

  public IllegalSyntaxException(String message) {
    super(message);
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.ERROR;
  }
}
