package core.procedures.io;

import core.Repl;
import core.exceptions.SCMIOException;
import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMOutputPort;
import core.scm.SCMMutableString;
import core.writer.Writer;

import java.io.IOException;

import static core.scm.SCMUnspecified.UNSPECIFIED;

@FnArgs(minArgs = 1, maxArgs = 2, mandatoryArgsTypes = {Object.class}, restArgsType = {SCMOutputPort.class})
public class Display extends AFn {

  @Override
  public String getName() {
    return "display";
  }

  @Override
  public Object apply(Object... args) {
    SCMOutputPort outputPort;
    if (args.length == 1) {
      outputPort = Repl.getCurrentOutputPort();
    } else {
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
