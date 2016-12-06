package core.procedures.io;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMInputPort;
import core.scm.SCMUnspecified;

import java.io.IOException;

@FnArgs(args = {SCMInputPort.class})
public class CloseInputPort extends AFn {

  @Override
  public String getName() {
    return "close-input-port";
  }

  @Override
  public Object invoke(Object... args) {
    try {
      ((SCMInputPort)args[0]).close();
    } catch (IOException e) {
      e.printStackTrace();
    }
    return SCMUnspecified.UNSPECIFIED;
  }
}
