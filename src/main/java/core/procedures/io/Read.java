package core.procedures.io;

import core.Repl;
import core.procedures.AFn;
import core.reader.Reader;
import core.scm.FnArgs;
import core.scm.SCMCons;
import core.scm.SCMInputPort;
import core.scm.specialforms.Begin;
import core.scm.SCMThunk;

import java.util.List;

@FnArgs(maxArgs = 1, restArgsType = SCMInputPort.class)
public final class Read extends AFn {

  @Override
  public String getName() {
    return "read";
  }

  @Override
  public Object apply(Object... args) {
    SCMInputPort inputPort;
    if (args.length == 0) {
      inputPort = Repl.getCurrentInputPort();
    } else {
      inputPort = ((SCMInputPort)args[0]);
    }
    List<Object> sexps = SCMCons.list(Begin.BEGIN);
    sexps.addAll(new Reader(inputPort.getInputStream()).read());
    return new SCMThunk(sexps, null);
  }
}
