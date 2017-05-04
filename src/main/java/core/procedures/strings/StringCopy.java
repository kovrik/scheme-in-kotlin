package core.procedures.strings;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMMutableString;

public final class StringCopy extends AFn {

  public StringCopy() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{CharSequence.class}).build());
  }

  @Override
  public String getName() {
    return "string-copy";
  }

  @Override
  public SCMMutableString apply1(Object arg) {
    return new SCMMutableString(arg.toString());
  }
}
