package core.procedures.strings;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

public final class Trim extends AFn {

  public Trim() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{CharSequence.class}).build());
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
