package core.exceptions;

import core.scm.ISCMClass;
import core.scm.SCMClass;

public class ArityException extends IllegalArgumentException implements ISCMClass {

  public ArityException(String name, int min, int max, int given) {
    super((name.isEmpty() ? "#<procedure>" : name) + ": arity mismatch; " +
           "the expected number of arguments does not match the given number " + "(" +
           "expected: " + ((min == max) ? min : ((max > 255) ? "at least " + min : min + " to " + max)) +
           ", given: " + given + ")", null);
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.ERROR;
  }
}
