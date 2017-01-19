package core.procedures.strings;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMMutableString;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {String.class})
public final class StringToMutableString extends AFn {

  @Override
  public String getName() {
    return "string->mutable-string";
  }

  @Override
  public Object apply1(Object arg) {
    if (arg instanceof SCMMutableString || arg instanceof StringBuilder) {
      return arg;
    } else {
      return new SCMMutableString(arg.toString());
    }
  }
}