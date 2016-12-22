package core.procedures.strings;

import core.procedures.AFn;
import core.scm.FnArgs;

@FnArgs(restArgsType = {String.class})
public class StringAppend extends AFn {

  @Override
  public String getName() {
    return "string-append";
  }

  @Override
  public String apply(Object... args) {
    if (args.length == 0) {
      return "";
    }
    if (args.length == 1) {
      Object o = args[0];
      return o.toString();
    }
    StringBuilder sb = new StringBuilder();
    for (Object str : args) {
      sb.append(str);
    }
    return sb.toString();
  }
}
