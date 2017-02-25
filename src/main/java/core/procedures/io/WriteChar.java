package core.procedures.io;

import core.Repl;
import core.exceptions.SCMIOException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMOutputPort;
import core.scm.SCMConstant;

import java.io.IOException;

public final class WriteChar extends AFn {

  public WriteChar() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(2).mandatoryArgsTypes(new Class[]{Character.class})
                                                   .restArgsType(SCMOutputPort.class));
  }

  @Override
  public String getName() {
    return "write-char";
  }

  @Override
  public Object apply(Object... args) {
    Character ch = (Character)args[0];
    SCMOutputPort outputPort;
    if (args.length == 1) {
      outputPort = Repl.getCurrentOutputPort();
    } else {
      outputPort = ((SCMOutputPort)args[1]);
    }
    try {
      outputPort.write(ch);
    } catch (IOException e) {
      throw new SCMIOException(e);
    }
    return SCMConstant.UNSPECIFIED;
  }
}
