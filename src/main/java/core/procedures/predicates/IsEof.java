package core.procedures.predicates;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.scm.SCMBoolean;
import core.scm.SCMEof;

public class IsEof extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "eof-object?";
  }

  @Override
  public SCMBoolean invoke(Object... args) {
    if (args.length != 1) {
      throw new ArityException(args.length, 1, getName());
    }
    return isNull(args[0]);
  }

  public static SCMBoolean isNull(Object object) {
    return SCMBoolean.toSCMBoolean(SCMEof.EOF.equals(object));
  }
}
