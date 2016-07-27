package core.procedures.strings;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;

public class StringRef extends AFn {

  @Override
  public String getName() {
    return "string-ref";
  }

  @Override
  public Object invoke(Object... args) {
    if (args.length != 2) {
      throw new ArityException(args.length, 2, getName());
    }

    Object o = args[0];
    if (!(o instanceof String)) {
      throw new WrongTypeException("String", o);
    }
    String s = (String)o;

    Object p = args[1];
    if (!(p instanceof Long)) {
      throw new WrongTypeException("Integer", p);
    }
    Long pos = (Long)p;
    if ((pos < 0) || (pos >= s.length())) {
      throw new IllegalArgumentException(String.format("Value out of range: %s", pos));
    }
    return s.charAt(pos.intValue());
  }
}
