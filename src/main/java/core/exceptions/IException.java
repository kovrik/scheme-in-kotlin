package core.exceptions;

import core.scm.ITyped;
import core.scm.Type;

public interface IException extends ITyped {

  @Override
  default Type getType() {
    return Type.ERROR;
  }
}
