package core.procedures.io;

import core.Repl;
import core.procedures.AFn;
import core.scm.FnArgs;

@FnArgs(maxArgs = 0)
public final class CurrentInputPort extends AFn {

  @Override
  public String getName() {
    return "current-input-port";
  }

  @Override
  public Object apply0() {
    return Repl.getCurrentInputPort();
  }
}
