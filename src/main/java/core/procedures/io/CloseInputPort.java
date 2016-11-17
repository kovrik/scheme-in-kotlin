package core.procedures.io;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMInputPort;
import core.scm.SCMUnspecified;

import java.io.IOException;

public class CloseInputPort extends AFn {

  @Override
  public String getName() {
    return "close-input-port";
  }

  @Override
  public Object invoke(Object... args) {
    if (args.length != 1) {
      throw new ArityException(args.length, 1, getName());
    }
    if (!(args[0] instanceof SCMInputPort)) {
      throw new WrongTypeException("Input Port", args[0]);
    }
    try {
      ((SCMInputPort)args[0]).close();
    } catch (IOException e) {
      e.printStackTrace();
    }
    return SCMUnspecified.UNSPECIFIED;
  }
}
