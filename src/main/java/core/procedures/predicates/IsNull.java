package core.procedures.predicates;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.scm.SCMBoolean;

import java.util.List;

public class IsNull extends AFn {

  @Override
  public String getName() {
    return "null?";
  }

  @Override
  public SCMBoolean invoke(Object... args) {
    if (args.length != 1) {
      throw new ArityException(args.length, 1, getName());
    }
    return isNull(args[0]);
  }

  public static SCMBoolean isNull(Object object) {
    if (object == null) {
      return SCMBoolean.TRUE;
    }
    if (object instanceof List) {
      return SCMBoolean.toSCMBoolean(((List)object).isEmpty());
    }
    return SCMBoolean.FALSE;
  }
}
