package core.procedures.system;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.Void;

public final class Exit extends AFn {

  public Exit() {
    super(new FnArgsBuilder().max(1).rest(Long.class).build());
  }

  @Override
  public String getName() {
    return "exit";
  }

  @Override
  public Void apply(Object... args) {
    if (args.length == 0) {
      System.exit(0);
    } else {
      System.exit(((Long)args[0]).intValue());
    }
    return Void.VOID;
  }
}
