package core.procedures.io;

import core.Main;
import core.exceptions.ArityException;
import core.exceptions.SCMIOException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMOutputPort;
import core.scm.SCMUnspecified;
import core.writer.Writer;

import java.io.IOException;

public class Write extends AFn {

  @Override
  public String getName() {
    return "write";
  }

  @Override
  public Object invoke(Object... args) {
    if (args.length < 1) {
      throw new ArityException(args.length, 1, getName());
    }
    if (args.length > 2) {
      throw new ArityException(args.length, 2, getName());
    }

    SCMOutputPort outputPort;
    if (args.length == 1) {
      outputPort = Main.getCurrentOutputPort();
    } else {
      if (!(args[1] instanceof SCMOutputPort)) {
        throw new WrongTypeException("Output Port", args[1]);
      }
      outputPort = ((SCMOutputPort)args[1]);
    }
    try {
      outputPort.write(Writer.write(args[0]));
    } catch (IOException e) {
      throw new SCMIOException(e);
    }
    return SCMUnspecified.UNSPECIFIED;
  }
}
