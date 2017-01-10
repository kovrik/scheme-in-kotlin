package core.procedures.io;

import core.Repl;
import core.exceptions.SCMIOException;
import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMInputPort;

import java.io.IOException;

@FnArgs(maxArgs = 1, restArgsType = SCMInputPort.class)
public class ReadChar extends AFn {

  @Override
  public String getName() {
    return "read-char";
  }

  @Override
  public Object apply(Object... args) {
    SCMInputPort inputPort;
    if (args.length == 0) {
      inputPort = Repl.getCurrentInputPort();
    } else {
      inputPort = ((SCMInputPort)args[0]);
    }
    try {
      return (char)inputPort.read();
    } catch (IOException e) {
      throw new SCMIOException(e);
    }
  }
}
