package core.procedures.io;

import core.Repl;
import core.procedures.AFn;
import core.scm.FnArgs;

@FnArgs(maxArgs = 0)
public class CurrentOutputPort extends AFn {

  @Override
  public String getName() {
    return "current-output-port";
  }

  @Override
  public Object apply0() {
    return Repl.getCurrentOutputPort();
  }
}
