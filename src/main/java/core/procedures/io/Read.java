package core.procedures.io;

import core.Repl;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.reader.Reader;
import core.scm.Cons;
import core.scm.InputPort;
import core.scm.specialforms.Begin;
import core.scm.Thunk;

import java.util.List;

public final class Read extends AFn {

  public Read() {
    super(new FnArgsBuilder().max(1).rest(InputPort.class).build());
  }

  @Override
  public String getName() {
    return "read";
  }

  @Override
  public Object apply(Object... args) {
    InputPort inputPort;
    if (args.length == 0) {
      inputPort = Repl.getCurrentInputPort();
    } else {
      inputPort = ((InputPort)args[0]);
    }
    List<Object> sexps = Cons.list(Begin.BEGIN);
    sexps.addAll(new Reader(inputPort.getInputStream()).read());
    return new Thunk(sexps);
  }
}
