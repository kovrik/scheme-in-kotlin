package core.procedures.io;

import core.Main;
import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.reader.Reader;
import core.scm.SCMCons;
import core.scm.SCMInputPort;
import core.scm.specialforms.Begin;
import core.scm.SCMTailCall;

import java.util.List;

public class Read extends AFn {

  @Override
  public String getName() {
    return "read";
  }

  @Override
  public Object apply(Object... args) {
    if (args.length > 2) {
      throw new ArityException(args.length, 2, getName());
    }

    SCMInputPort inputPort;
    if (args.length == 0) {
      inputPort = Main.getCurrentInputPort();
    } else {
      if (!(args[0] instanceof SCMInputPort)) {
        throw new WrongTypeException("Input Port", args[0]);
      }
      inputPort = ((SCMInputPort)args[0]);
    }
    List<Object> sexps = SCMCons.list(Begin.BEGIN);
    sexps.addAll(new Reader(inputPort.getInputStream()).read());
    return new SCMTailCall(sexps, null);
  }
}
