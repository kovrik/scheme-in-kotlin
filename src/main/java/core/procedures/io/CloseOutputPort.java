package core.procedures.io;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMOutputPort;
import core.scm.SCMUnspecified;

import java.io.IOException;

@FnArgs(args = {SCMOutputPort.class})
public class CloseOutputPort extends AFn {

  @Override
  public String getName() {
    return "close-output-port";
  }

  @Override
  public Object invoke(Object... args) {
    try {
      ((SCMOutputPort)args[0]).close();
    } catch (IOException e) {
      e.printStackTrace();
    }
    return SCMUnspecified.UNSPECIFIED;
  }
}
