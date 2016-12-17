package core.procedures.io;

import core.Repl;
import core.exceptions.ArityException;
import core.exceptions.SCMIOException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMOutputPort;

import java.io.IOException;

import static core.scm.SCMUnspecified.UNSPECIFIED;

public class Newline extends AFn {

  private static final String LS = System.getProperty("line.separator");

  @Override
  public String getName() {
    return "newline";
  }

  @Override
  public Object apply(Object... args) {
    if (args.length > 1) {
      throw new ArityException(args.length, 1, getName());
    }

    SCMOutputPort outputPort;
    if (args.length == 0) {
      outputPort = Repl.getCurrentOutputPort();
    } else {
      if (!(args[0] instanceof SCMOutputPort)) {
        throw new WrongTypeException("Output Port", args[0]);
      }
      outputPort = ((SCMOutputPort)args[0]);
    }
    try {
      outputPort.write(LS);
    } catch (IOException e) {
      throw new SCMIOException(e);
    }
    return UNSPECIFIED;
  }
}
