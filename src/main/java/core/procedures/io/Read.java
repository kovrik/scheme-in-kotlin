package core.procedures.io;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.reader.IReader;
import core.reader.Reader;
import core.scm.SCMCons;
import core.scm.specialforms.Begin;
import core.scm.specialforms.TailCall;

import java.util.List;

public class Read extends AFn {

  private final IReader reader = new Reader();

  @Override
  public String getName() {
    return "read";
  }

  @Override
  public Object invoke(Object... args) {
    // TODO Read input-port as first arg
    if (args.length > 0) {
      throw new ArityException(args.length, 0, getName());
    }
    List<Object> sexps = SCMCons.list(Begin.BEGIN);
    sexps.addAll(reader.read(System.in));
    return new TailCall(sexps, null);
  }
}
