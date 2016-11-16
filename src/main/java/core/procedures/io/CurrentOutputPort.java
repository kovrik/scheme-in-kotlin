package core.procedures.io;

import core.Main;
import core.exceptions.ArityException;
import core.procedures.AFn;

public class CurrentOutputPort extends AFn {

  @Override
  public String getName() {
    return "current-output-port";
  }

  @Override
  public Object invoke(Object... args) {
    if (args.length != 0) {
      throw new ArityException(args.length, 0, getName());
    }
    return Main.getCurrentOutputPort();
  }
}
