package core.procedures.io;

import core.Repl;
import core.exceptions.SCMIOException;
import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBoolean;
import core.scm.SCMInputPort;

import java.io.IOException;

// FIXME Doesn't work properly
@FnArgs(maxArgs = 1, restArgsType = {SCMInputPort.class})
public class IsCharReady extends AFn {

  @Override
  public String getName() {
    return "char-ready?";
  }

  @Override
  public Object apply(Object... args) {
    SCMInputPort inputPort;
    if (args.length == 0) {
      inputPort = Repl.getCurrentInputPort();
    } else {
      inputPort = ((SCMInputPort)args[0]);
    }
    int bytesAvailable;
    try {
      bytesAvailable = inputPort.available();
    } catch (IOException e) {
      throw new SCMIOException(e);
    }
    return SCMBoolean.toSCMBoolean(bytesAvailable > 0);
  }
}
