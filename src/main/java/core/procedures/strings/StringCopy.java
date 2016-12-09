package core.procedures.strings;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMMutableString;

@FnArgs(args = {String.class})
public class StringCopy extends AFn {

  @Override
  public String getName() {
    return "string-copy";
  }

  @Override
  public SCMMutableString invoke(Object... args) {
    return new SCMMutableString(args[0].toString());
  }
}
