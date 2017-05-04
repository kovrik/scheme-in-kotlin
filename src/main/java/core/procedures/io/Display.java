package core.procedures.io;

import core.Repl;
import core.exceptions.SCMIOException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.OutputPort;
import core.scm.Void;
import core.writer.Writer;

import java.io.IOException;

public class Display extends AFn {

  public Display() {
    super(new FnArgsBuilder().min(1).max(2).mandatory(new Class[]{Object.class})
                             .rest(OutputPort.class).build());
  }

  @Override
  public String getName() {
    return "display";
  }

  @Override
  public Object apply(Object... args) {
    OutputPort outputPort;
    if (args.length == 1) {
      outputPort = Repl.getCurrentOutputPort();
    } else {
      outputPort = ((OutputPort)args[1]);
    }
    Object arg = args[0];
    try {
      if ((arg instanceof CharSequence) || (arg instanceof Character)) {
        outputPort.write(arg.toString());
      } else {
        outputPort.write(Writer.write(arg));
      }
    } catch (IOException e) {
      throw new SCMIOException(e);
    }
    return Void.VOID;
  }
}
