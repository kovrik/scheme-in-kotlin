package core.procedures.strings;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

public final class StringToImmutableString extends AFn {

  public StringToImmutableString() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{CharSequence.class}).build());
  }

  @Override
  public String getName() {
    return "string->immutable-string";
  }

  @Override
  public Object apply1(Object arg) {
    if (arg instanceof String) {
      return arg;
    } else {
      return arg.toString();
    }
  }
}