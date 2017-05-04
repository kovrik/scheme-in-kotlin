package core.procedures.strings;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

public final class EndsWith extends AFn {

  public EndsWith() {
    super(new FnArgsBuilder().min(2).max(2).mandatory(new Class[]{CharSequence.class, CharSequence.class}).build());
  }

  @Override
  public String getName() {
    return "ends-with?";
  }

  @Override
  public Boolean apply2(Object arg1, Object arg2) {
    return arg1.toString().endsWith(arg2.toString());
  }
}
