package core.procedures.io;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMOutputPort;
import core.scm.SCMConstant;

import java.io.IOException;

public final class CloseOutputPort extends AFn {

  public CloseOutputPort() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{SCMOutputPort.class}));
  }

  @Override
  public String getName() {
    return "close-output-port";
  }

  @Override
  public Object apply(Object... args) {
    try {
      ((SCMOutputPort)args[0]).close();
    } catch (IOException e) {
      e.printStackTrace();
    }
    return SCMConstant.UNSPECIFIED;
  }
}
