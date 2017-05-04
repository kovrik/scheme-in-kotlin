package core.procedures.strings;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.MutableString;

public final class StringToMutableString extends AFn {

  public StringToMutableString() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{CharSequence.class}).build());
  }

  @Override
  public String getName() {
    return "string->mutable-string";
  }

  @Override
  public Object apply1(Object arg) {
    if (arg instanceof MutableString || arg instanceof StringBuilder) {
      return arg;
    } else {
      return new MutableString(arg.toString());
    }
  }
}