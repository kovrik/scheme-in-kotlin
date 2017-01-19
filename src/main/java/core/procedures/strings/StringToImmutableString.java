package core.procedures.strings;

import core.procedures.AFn;
import core.scm.FnArgs;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {String.class})
public final class StringToImmutableString extends AFn {

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