package core.procedures.io;

import core.Main;
import core.exceptions.ArityException;
import core.exceptions.SCMIOException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMOutputPort;
import core.scm.SCMUnspecified;

import java.io.IOException;

public class WriteChar extends AFn {

  @Override
  public String getName() {
    return "write-char";
  }

  @Override
  public Object apply(Object... args) {
    if (args.length < 1) {
      throw new ArityException(args.length, 1, getName());
    }
    if (args.length > 2) {
      throw new ArityException(args.length, 2, getName());
    }
    if (!(args[0] instanceof Character)) {
      throw new WrongTypeException("Character", args[0]);
    }
    Character ch = (Character)args[0];

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
      outputPort.write(ch);
    } catch (IOException e) {
      throw new SCMIOException(e);
    }
    return SCMUnspecified.UNSPECIFIED;
  }
}
