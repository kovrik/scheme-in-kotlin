package core.procedures.strings;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMMutableString;

@FnArgs(isVariadic = true)
public class StringProc extends AFn {

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
      if (!(c instanceof Character)) {
        throw new WrongTypeException("Character", c);
      }
      string.append(c);
    }
    return string;
  }
}
