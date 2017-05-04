package core.procedures.io;

import core.Repl;
import core.exceptions.SCMIOException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMOutputPort;
import core.scm.SCMVoid;
import core.writer.Writer;

import java.io.IOException;

public final class Println extends AFn {

  public Println() {
    super(new FnArgsBuilder().min(1).max(2).mandatory(new Class[]{Object.class})
                             .rest(SCMOutputPort.class).build());
  }

  @Override
  public String getName() {
    return "println";
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
      if ((arg instanceof CharSequence) || (arg instanceof Character)) {
        outputPort.writeln(arg.toString());
      } else {
        outputPort.writeln(Writer.write(arg));
      }
    } catch (IOException e) {
      throw new SCMIOException(e);
    }
    return SCMVoid.VOID;
  }
}
