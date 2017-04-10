package core.procedures.io;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMInputPort;
import core.scm.SCMVoid;

import java.io.IOException;

public final class CloseInputPort extends AFn {

  public CloseInputPort() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{SCMInputPort.class}));
  }

  @Override
  public String getName() {
    return "close-input-port";
  }

  @Override
  public SCMVoid apply(Object... args) {
    try {
      ((SCMInputPort)args[0]).close();
    } catch (IOException e) {
      e.printStackTrace();
    }
    return SCMVoid.VOID;
  }
}
