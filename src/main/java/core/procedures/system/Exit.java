package core.procedures.system;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMVoid;

public final class Exit extends AFn {

  public Exit() {
    super(new FnArgsBuilder().maxArgs(1).restArgsType(Long.class));
  }

  @Override
  public String getName() {
    return "exit";
  }

  @Override
  public SCMVoid apply(Object... args) {
    if (args.length == 0) {
      System.exit(0);
    } else {
      System.exit(((Long)args[0]).intValue());
    }
    return SCMVoid.VOID;
  }
}
