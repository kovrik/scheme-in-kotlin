package core.procedures.io;

import core.Repl;
import core.exceptions.SCMIOException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.InputPort;

import java.io.IOException;

public final class ReadChar extends AFn {

  public ReadChar() {
    super(new FnArgsBuilder().max(1).rest(InputPort.class).build());
  }

  @Override
  public String getName() {
    return "read-char";
  }

  @Override
  public Object apply(Object... args) {
    InputPort inputPort;
    if (args.length == 0) {
      inputPort = Repl.getCurrentInputPort();
    } else {
      inputPort = ((InputPort)args[0]);
    }
    try {
      return (char)inputPort.read();
    } catch (IOException e) {
      throw new SCMIOException(e);
    }
  }
}
