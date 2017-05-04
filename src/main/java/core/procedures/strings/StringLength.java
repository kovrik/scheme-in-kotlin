package core.procedures.strings;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

public final class StringLength extends AFn {

  public StringLength() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{CharSequence.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "string-length";
  }

  @Override
  public Long apply1(Object arg) {
    return ((Integer)(arg.toString()).length()).longValue();
  }
}
