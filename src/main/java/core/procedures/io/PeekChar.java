package core.procedures.io;

import core.Repl;
import core.exceptions.SCMIOException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.InputPort;

import java.io.IOException;

public final class PeekChar extends AFn {

  public PeekChar() {
    super(new FnArgsBuilder().max(1).rest(InputPort.class).build());
  }

  @Override
  public String getName() {
    return "peek-char";
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
      return (char)inputPort.peek();
    } catch (IOException e) {
      throw new SCMIOException(e);
    }
  }
}
