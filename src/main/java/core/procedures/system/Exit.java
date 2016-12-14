package core.procedures.system;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.FnArgs;

import static core.scm.SCMUnspecified.UNSPECIFIED;

@FnArgs(isVariadic = true)
public class Exit extends AFn {

  @Override
  public String getName() {
    return "exit";
  }

  @Override
  public Object apply(Object... args) {
    if (args.length == 0) {
      System.exit(0);
    } else {
      Object o = args[0];
      if (!(args[0] instanceof Long)) {
        throw new WrongTypeException("Integer", o);
      }
      System.exit(((Long)o).intValue());
    }
    return UNSPECIFIED;
  }
}
