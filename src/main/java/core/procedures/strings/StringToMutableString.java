package core.procedures.strings;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMMutableString;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {String.class})
public class StringToMutableString extends AFn {

  @Override
  public String getName() {
    return "string->mutable-string";
  }

  @Override
  public Object apply(Object... args) {
    if (args[0] instanceof SCMMutableString || args[0] instanceof StringBuilder) {
      return args[0];
    } else {
      return new SCMMutableString(args[0].toString());
    }
  }
}