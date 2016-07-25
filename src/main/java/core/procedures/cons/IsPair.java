package core.procedures.cons;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.scm.ICons;
import core.scm.SCMBoolean;

import java.util.List;

public class IsPair extends AFn {

  @Override
  public SCMBoolean invoke(Object... args) {
    if (args.length != 1) {
      throw new ArityException(args.length, 1, "null?");
    }
    return isPair(args[0]);
  }

  public static SCMBoolean isPair(Object object) {
    if (object instanceof ICons) {
      return SCMBoolean.toSCMBoolean(((ICons)object).isPair());
    }
    if (object instanceof List) {
      return SCMBoolean.toSCMBoolean(!((List)object).isEmpty());
    }
    return SCMBoolean.FALSE;
  }
}
