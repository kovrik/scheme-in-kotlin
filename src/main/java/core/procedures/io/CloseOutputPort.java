package core.procedures.io;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMOutputPort;
import core.scm.SCMVoid;

import java.io.IOException;

public final class CloseOutputPort extends AFn {

  public CloseOutputPort() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{SCMOutputPort.class}).build());
  }

  @Override
  public String getName() {
    return "close-output-port";
  }

  @Override
  public SCMVoid apply(Object... args) {
    try {
      ((SCMOutputPort)args[0]).close();
    } catch (IOException e) {
      e.printStackTrace();
    }
    return SCMVoid.VOID;
  }
}
