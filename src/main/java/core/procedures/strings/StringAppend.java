package core.procedures.strings;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMMutableString;

@FnArgs(isVariadic = true)
public class StringAppend extends AFn {

  @Override
  public String getName() {
    return "string-append";
  }

  @Override
  public String invoke(Object... args) {
    if (args.length == 0) {
      return "";
    }
    if (args.length == 1) {
      Object o = args[0];
      if (!(o instanceof String || o instanceof SCMMutableString)) {
        throw new WrongTypeException("String", o);
      }
      return o.toString();
    }
    StringBuilder sb = new StringBuilder();
    for (Object str : args) {
      if (!(str instanceof String || str instanceof SCMMutableString)) {
        throw new WrongTypeException("String", str);
      }
      sb.append(str);
    }
    return sb.toString();
  }
}
