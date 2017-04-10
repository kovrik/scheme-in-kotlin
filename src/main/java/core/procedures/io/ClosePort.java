package core.procedures.io;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.ISCMPort;
import core.scm.SCMVoid;

import java.io.IOException;

public final class ClosePort extends AFn {

  public ClosePort() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{ISCMPort.class}));
  }

  @Override
  public String getName() {
    return "close-port";
  }

  @Override
  public SCMVoid apply(Object... args) {
    try {
      ((ISCMPort)args[0]).close();
    } catch (IOException e) {
      e.printStackTrace();
    }
    return SCMVoid.VOID;
  }
}
