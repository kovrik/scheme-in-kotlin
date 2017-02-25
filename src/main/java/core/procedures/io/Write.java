package core.procedures.io;

import core.Repl;
import core.exceptions.SCMIOException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMOutputPort;
import core.scm.SCMConstant;
import core.writer.Writer;

import java.io.IOException;

public final class Write extends AFn {

  public Write() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(2).mandatoryArgsTypes(new Class[]{Object.class})
                             .restArgsType(SCMOutputPort.class));
  }

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
    return SCMConstant.UNSPECIFIED;
  }
}
