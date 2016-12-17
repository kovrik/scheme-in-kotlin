package core.procedures.io;

import core.Repl;
import core.exceptions.ArityException;
import core.exceptions.SCMIOException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMInputPort;

import java.io.IOException;

public class ReadChar extends AFn {

  @Override
  public String getName() {
    return "read-char";
  }

  @Override
  public Object apply(Object... args) {
    if (args.length > 1) {
      throw new ArityException(args.length, 1, getName());
    }
    SCMInputPort inputPort;
    if (args.length == 0) {
      inputPort = Repl.getCurrentInputPort();
    } else {
      if (!(args[0] instanceof SCMInputPort)) {
        throw new WrongTypeException("Input Port", args[0]);
      }
      inputPort = ((SCMInputPort)args[0]);
    }
    try {
      return (char)inputPort.read();
    } catch (IOException e) {
      throw new SCMIOException(e);
    }
  }
}
