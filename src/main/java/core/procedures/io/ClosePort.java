package core.procedures.io;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.ISCMPort;
import core.scm.SCMConstant;

import java.io.IOException;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = ISCMPort.class)
public final class ClosePort extends AFn {

  @Override
  public String getName() {
    return "close-port";
  }

  @Override
  public Object apply(Object... args) {
    try {
      ((ISCMPort)args[0]).close();
    } catch (IOException e) {
      e.printStackTrace();
    }
    return SCMConstant.UNSPECIFIED;
  }
}
