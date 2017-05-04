package core.procedures.io;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.IPort;
import core.scm.Void;

import java.io.IOException;

public final class ClosePort extends AFn {

  public ClosePort() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{IPort.class}).build());
  }

  @Override
  public String getName() {
    return "close-port";
  }

  @Override
  public Void apply(Object... args) {
    try {
      ((IPort)args[0]).close();
    } catch (IOException e) {
      e.printStackTrace();
    }
    return Void.VOID;
  }
}
