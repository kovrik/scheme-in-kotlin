package core.procedures.io;

import core.Repl;
import core.exceptions.SCMIOException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.InputPort;

import java.io.IOException;

public final class IsCharReady extends AFn {

  public IsCharReady() {
    super(new FnArgsBuilder().max(1).rest(InputPort.class).build());
  }

  @Override
  public String getName() {
    return "char-ready?";
  }

  @Override
  public Boolean apply(Object... args) {
    InputPort inputPort;
    if (args.length == 0) {
      inputPort = Repl.getCurrentInputPort();
    } else {
      inputPort = ((InputPort)args[0]);
    }
    int bytesAvailable;
    try {
      bytesAvailable = inputPort.available();
    } catch (IOException e) {
      throw new SCMIOException(e);
    }
    return bytesAvailable > 0;
  }
}
