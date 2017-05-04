package core.procedures.io;

import core.Repl;
import core.exceptions.SCMIOException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.OutputPort;
import core.writer.Writer;

import java.io.IOException;

public final class Write extends AFn {

  public Write() {
    super(new FnArgsBuilder().min(1).max(2).mandatory(new Class[]{Object.class})
                             .rest(OutputPort.class).build());
  }

  @Override
  public String getName() {
    return "write";
  }

  @Override
  public Object apply(Object... args) {
    OutputPort outputPort;
    if (args.length == 1) {
      outputPort = Repl.getCurrentOutputPort();
    } else {
      outputPort = ((OutputPort)args[1]);
    }
    try {
      outputPort.write(Writer.write(args[0]));
    } catch (IOException e) {
      throw new SCMIOException(e);
    }
    return null;
  }
}
