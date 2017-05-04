package core.procedures.io;

import core.Repl;
import core.exceptions.SCMIOException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.OutputPort;
import core.scm.Void;

import java.io.IOException;

public final class Newline extends AFn {

  private static final String LS = System.getProperty("line.separator");

  public Newline() {
    super(new FnArgsBuilder().max(1).rest(OutputPort.class).build());
  }

  @Override
  public String getName() {
    return "newline";
  }

  @Override
  public Object apply(Object... args) {
    OutputPort outputPort;
    if (args.length == 0) {
      outputPort = Repl.getCurrentOutputPort();
    } else {
      outputPort = ((OutputPort)args[0]);
    }
    try {
      outputPort.write(LS);
    } catch (IOException e) {
      throw new SCMIOException(e);
    }
    return Void.VOID;
  }
}
