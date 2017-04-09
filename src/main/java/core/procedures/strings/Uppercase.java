package core.procedures.strings;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

public final class Uppercase extends AFn {

  public Uppercase() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{CharSequence.class}));
  }

  @Override
  public String getName() {
    return "upper-case";
  }

  @Override
  public String apply1(Object arg) {
    return arg.toString().toUpperCase();
  }
}
