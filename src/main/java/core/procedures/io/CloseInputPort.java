package core.procedures.io;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.InputPort;
import core.scm.Void;

import java.io.IOException;

public final class CloseInputPort extends AFn {

  public CloseInputPort() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{InputPort.class}).build());
  }

  @Override
  public String getName() {
    return "close-input-port";
  }

  @Override
  public Void apply(Object... args) {
    try {
      ((InputPort)args[0]).close();
    } catch (IOException e) {
      e.printStackTrace();
    }
    return Void.VOID;
  }
}
