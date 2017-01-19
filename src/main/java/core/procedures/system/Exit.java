package core.procedures.system;

import core.procedures.AFn;
import core.scm.FnArgs;

import static core.scm.SCMUnspecified.UNSPECIFIED;

@FnArgs(maxArgs = 1, restArgsType = {Long.class})
public final class Exit extends AFn {

  @Override
  public String getName() {
    return "exit";
  }

  @Override
  public Object apply(Object... args) {
    if (args.length == 0) {
      System.exit(0);
    } else {
      System.exit(((Long)args[0]).intValue());
    }
    return UNSPECIFIED;
  }
}
