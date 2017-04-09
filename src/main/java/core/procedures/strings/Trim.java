package core.procedures.strings;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

public final class Trim extends AFn {

  public Trim() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{CharSequence.class}));
  }

  @Override
  public String getName() {
    return "trim";
  }

  @Override
  public String apply1(Object arg) {
    return arg.toString().trim();
  }
}
