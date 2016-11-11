package core.procedures.io;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMUnspecified;

public class WriteChar extends AFn {

  @Override
  public String getName() {
    return "write-char";
  }

  @Override
  public Object invoke(Object... args) {
    // TODO Read input-port as second arg
    if (args.length != 1) {
      throw new ArityException(args.length, 1, getName());
    }
    if (!(args[0] instanceof Character)) {
      throw new WrongTypeException("Character", args[0]);
    }
    System.out.print(args[0]);
    return SCMUnspecified.UNSPECIFIED;
  }
}
