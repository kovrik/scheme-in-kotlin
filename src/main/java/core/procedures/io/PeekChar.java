package core.procedures.io;

import core.Repl;
import core.exceptions.SCMIOException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMInputPort;

import java.io.IOException;

public final class PeekChar extends AFn {

  public PeekChar() {
    super(new FnArgsBuilder().max(1).rest(SCMInputPort.class).build());
  }

  @Override
  public String getName() {
    return "peek-char";
  }

  @Override
  public Object apply(Object... args) {
    SCMInputPort inputPort;
    if (args.length == 0) {
      inputPort = Repl.getCurrentInputPort();
    } else {
      inputPort = ((SCMInputPort)args[0]);
    }
    try {
      return (char)inputPort.peek();
    } catch (IOException e) {
      throw new SCMIOException(e);
    }
  }
}
