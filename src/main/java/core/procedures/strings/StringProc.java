package core.procedures.strings;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMMutableString;

@FnArgs(restArgsType = {Character.class})
public final class StringProc extends AFn {

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
