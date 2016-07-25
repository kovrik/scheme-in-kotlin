package core.procedures.strings;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;

public class StringAppend extends AFn {

  @Override
  public String invoke(Object... args) {
    if (args.length == 0) {
      return "";
    }
    if (args.length == 1) {
      Object o = args[0];
      if (!(o instanceof String)) {
        throw new WrongTypeException("String", o);
      }
      return (String)o;
    }
    StringBuilder sb = new StringBuilder();
    for (Object str : args) {
      if (!(str instanceof String)) {
        throw new WrongTypeException("String", str);
      }
      sb.append(str);
    }
    return sb.toString();
  }
}
