package core.procedures.strings;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMMutableString;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {String.class})
public class StringCopy extends AFn {

  @Override
  public String getName() {
    return "string-copy";
  }

  @Override
  public SCMMutableString apply1(Object arg) {
    return new SCMMutableString(arg.toString());
  }
}
