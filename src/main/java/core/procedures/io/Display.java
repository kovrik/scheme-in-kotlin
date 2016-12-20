package core.procedures.io;

import core.Repl;
import core.exceptions.ArityException;
import core.exceptions.SCMIOException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMOutputPort;
import core.scm.SCMMutableString;
import core.writer.Writer;

import java.io.IOException;

import static core.scm.SCMUnspecified.UNSPECIFIED;

public class Display extends AFn {

  @Override
  public String getName() {
    return "display";
  }

  @Override
  public Object apply(Object... args) {
    if (args.length < 1) {
      throw new ArityException(args.length, 1, getName());
    }
    if (args.length > 2) {
      throw new ArityException(args.length, 2, getName());
    }

    SCMOutputPort outputPort;
    if (args.length == 1) {
      outputPort = Repl.getCurrentOutputPort();
    } else {
      if (!(args[1] instanceof SCMOutputPort)) {
        throw new WrongTypeException("Output Port", args[1]);
      }
      outputPort = ((SCMOutputPort)args[1]);
    }

    Object arg = args[0];
    try {
      if ((arg instanceof String) || (arg instanceof SCMMutableString)) {
        outputPort.write(arg.toString());
      } else if (arg instanceof Character) {
        outputPort.write(arg.toString());
      } else {
        outputPort.write(Writer.write(arg));
      }
    } catch (IOException e) {
      throw new SCMIOException(e);
    }
    return UNSPECIFIED;
  }
}
