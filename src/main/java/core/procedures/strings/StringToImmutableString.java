package core.procedures.strings;

import core.procedures.AFn;
import core.scm.FnArgs;

@FnArgs(args = {String.class})
public class StringToImmutableString extends AFn {

  @Override
  public String getName() {
    return "string->immutable-string";
  }

  @Override
  public Object apply(Object... args) {
    if (args[0] instanceof String) {
      return args[0];
    } else {
      return args[0].toString();
    }
  }
}