package core.procedures.io;

import core.Repl;
import core.exceptions.SCMIOException;
import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMOutputPort;
import core.scm.SCMUnspecified;
import core.writer.Writer;

import java.io.IOException;

@FnArgs(minArgs = 1, maxArgs = 2, mandatoryArgsTypes = {Object.class}, restArgsType = {SCMOutputPort.class})
public class Write extends AFn {

  @Override
  public String getName() {
    return "write";
  }

  @Override
  public Object apply(Object... args) {
    SCMOutputPort outputPort;
    if (args.length == 1) {
      outputPort = Repl.getCurrentOutputPort();
    } else {
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
