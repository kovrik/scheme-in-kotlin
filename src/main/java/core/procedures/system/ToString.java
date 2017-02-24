package core.procedures.system;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.writer.Writer;

public final class ToString extends AFn {

  public ToString() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "->string";
  }

  @Override
  public Object apply1(Object arg) {
    return Writer.write(arg);
  }
}
