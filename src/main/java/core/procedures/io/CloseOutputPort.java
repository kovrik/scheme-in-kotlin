package core.procedures.io;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMOutputPort;
import core.scm.SCMUnspecified;

import java.io.IOException;

public class CloseOutputPort extends AFn {

  @Override
  public String getName() {
    return "close-output-port";
  }

  @Override
  public Object invoke(Object... args) {
    if (args.length != 1) {
      throw new ArityException(args.length, 1, getName());
    }
    if (!(args[0] instanceof SCMOutputPort)) {
      throw new WrongTypeException("Output Port", args[0]);
    }
    try {
      ((SCMOutputPort)args[0]).close();
    } catch (IOException e) {
      e.printStackTrace();
    }
    return SCMUnspecified.UNSPECIFIED;
  }
}
