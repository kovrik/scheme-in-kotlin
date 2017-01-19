package core.procedures.io;

import core.Repl;
import core.exceptions.SCMIOException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMOutputPort;
import core.scm.SCMUnspecified;

import java.io.IOException;

@FnArgs(minArgs = 1, maxArgs = 2, mandatoryArgsTypes = {Character.class}, restArgsType = SCMOutputPort.class)
public final class WriteChar extends AFn {

  @Override
  public String getName() {
    return "write-char";
  }

  @Override
  public Object apply(Object... args) {
    Character ch = (Character)args[0];
    SCMOutputPort outputPort;
    if (args.length == 1) {
      outputPort = Repl.getCurrentOutputPort();
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
