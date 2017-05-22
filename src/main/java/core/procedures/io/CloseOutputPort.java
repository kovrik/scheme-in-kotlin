package core.procedures.io;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.OutputPort;
import core.scm.Void;

import java.io.IOException;

public final class CloseOutputPort extends AFn {

  public CloseOutputPort() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{OutputPort.class}).build());
  }

  @Override
  public String getName() {
    return "close-output-port";
  }

  @Override
  public Void apply1(Object arg) {
    try {
      ((OutputPort)arg).close();
    } catch (IOException e) {
      e.printStackTrace();
    }
    return Void.VOID;
  }
}
