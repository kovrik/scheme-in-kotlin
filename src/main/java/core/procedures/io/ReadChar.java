package core.procedures.io;

import core.Repl;
import core.exceptions.SCMIOException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMInputPort;

import java.io.IOException;

public final class ReadChar extends AFn {

  public ReadChar() {
    super(new FnArgsBuilder().maxArgs(1).restArgsType(new Class[]{SCMInputPort.class}));
  }

  @Override
  public String getName() {
    return "read-char";
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
      return (char)inputPort.read();
    } catch (IOException e) {
      throw new SCMIOException(e);
    }
  }
}
