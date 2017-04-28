package core.exceptions;

import core.scm.ISCMClass;
import core.scm.SCMClass;

public interface ISCMException extends ISCMClass {

  @Override
  default SCMClass getSCMClass() {
    return SCMClass.ERROR;
  }
}
