package core.procedures.strings;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMString;

public class StringCopy extends AFn {

  @Override
  public String getName() {
    return "string-copy";
  }

  @Override
  public String invoke(Object... args) {
    if (args != null && args.length == 1) {
      if (args[0] instanceof String || args[0] instanceof SCMString) {
        return args[0].toString();
      }
      throw new WrongTypeException("String", args[0]);
    }
    throw new ArityException(args.length, 1, getName());
  }
}
