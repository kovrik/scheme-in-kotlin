package core.procedures.strings;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMMutableString;

public final class StringProc extends AFn {

  public StringProc() {
    super(new FnArgsBuilder().restArgsType(new Class[]{Character.class}));
  }

  @Override
  public String getName() {
    return "string";
  }

  @Override
  public SCMMutableString apply(Object... args) {
    if (args.length == 0) {
      return new SCMMutableString();
    }
    SCMMutableString string = new SCMMutableString(args.length);
    for (Object c : args) {
      string.append(c);
    }
    return string;
  }
}
