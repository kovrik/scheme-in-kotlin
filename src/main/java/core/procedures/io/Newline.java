package core.procedures.io;

import core.Repl;
import core.exceptions.SCMIOException;
import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMOutputPort;

import java.io.IOException;

import static core.scm.SCMUnspecified.UNSPECIFIED;

@FnArgs(maxArgs = 1, restArgsType = {SCMOutputPort.class})
public class Newline extends AFn {

  private static final String LS = System.getProperty("line.separator");

  @Override
  public String getName() {
    return "newline";
  }

  @Override
  public Object apply(Object... args) {
    SCMOutputPort outputPort;
    if (args.length == 0) {
      outputPort = Repl.getCurrentOutputPort();
    } else {
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
