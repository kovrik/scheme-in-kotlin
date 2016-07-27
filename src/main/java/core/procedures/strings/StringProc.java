package core.procedures.strings;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;

public class StringProc extends AFn {

  @Override
  public String getName() {
    return "string";
  }

  @Override
  public String invoke(Object... args) {
    if (args.length == 0) {
      return "";
    }
    StringBuilder sb = new StringBuilder(args.length);
    for (Object c : args) {
      if (!(c instanceof Character)) {
        throw new WrongTypeException("Character", c);
      }
      sb.append(c);
    }
    return sb.toString();
  }
}
