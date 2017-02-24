package core.procedures.io;

import core.Repl;
import core.exceptions.SCMIOException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMMutableString;
import core.scm.SCMOutputPort;
import core.writer.Writer;

import java.io.IOException;

import static core.scm.SCMConstant.UNSPECIFIED;

public final class Display extends AFn {

  public Display() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(2).mandatoryArgsTypes(new Class[]{Object.class})
                                                   .restArgsType(new Class[]{SCMOutputPort.class}));
  }

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
      if ((arg instanceof String) || (arg instanceof SCMMutableString) || (arg instanceof Character)) {
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
