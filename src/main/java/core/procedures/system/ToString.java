package core.procedures.system;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.writer.Writer;

@FnArgs(minArgs = 1, maxArgs = 1)
public class ToString extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "->string";
  }

  @Override
  public Object apply(Object... args) {
    return Writer.write(args[0]);
  }
}
