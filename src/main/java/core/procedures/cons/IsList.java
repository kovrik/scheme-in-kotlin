package core.procedures.cons;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.scm.ICons;
import core.scm.SCMBoolean;

import java.util.List;

public class IsList extends AFn {

  @Override
  public String getName() {
    return "list?";
  }

  @Override
  public SCMBoolean invoke(Object... args) {
    if (args.length != 1) {
      throw new ArityException(args.length, 1, getName());
    }
    return isList(args[0]);
  }

  public static SCMBoolean isList(Object object) {
    if (object instanceof ICons) {
      return SCMBoolean.toSCMBoolean(((ICons)object).isList());
    }
    return SCMBoolean.toSCMBoolean(object instanceof List);
  }
}
