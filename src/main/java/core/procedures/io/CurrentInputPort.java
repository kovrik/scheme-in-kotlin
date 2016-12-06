package core.procedures.io;

import core.Main;
import core.procedures.AFn;
import core.scm.FnArgs;

@FnArgs(args = {})
public class CurrentInputPort extends AFn {

  @Override
  public String getName() {
    return "current-input-port";
  }

  @Override
  public Object invoke(Object... args) {
    return Main.getCurrentInputPort();
  }
}
