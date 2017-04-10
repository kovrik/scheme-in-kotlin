package core.procedures.io;

import core.Repl;
import core.exceptions.SCMIOException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMOutputPort;
import core.scm.SCMVoid;

import java.io.IOException;

public final class Newline extends AFn {

  private static final String LS = System.getProperty("line.separator");

  public Newline() {
    super(new FnArgsBuilder().maxArgs(1).restArgsType(SCMOutputPort.class));
  }

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
    return SCMVoid.VOID;
  }
}
