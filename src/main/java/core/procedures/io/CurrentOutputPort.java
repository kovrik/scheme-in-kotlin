package core.procedures.io;

import core.Repl;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

public final class CurrentOutputPort extends AFn {

  public CurrentOutputPort() {
    super(new FnArgsBuilder().max(0).build());
  }

  @Override
  public String getName() {
    return "current-output-port";
  }

  @Override
  public Object apply0() {
    return Repl.getCurrentOutputPort();
  }
}
