package core.procedures.io;

import core.Main;
import core.exceptions.ArityException;
import core.procedures.AFn;

public class CurrentInputPort extends AFn {

  @Override
  public String getName() {
    return "current-input-port";
  }

  @Override
  public Object invoke(Object... args) {
    if (args.length != 0) {
      throw new ArityException(args.length, 0, getName());
    }
    return Main.getCurrentInputPort();
  }
}
