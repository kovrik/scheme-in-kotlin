package core.procedures.strings;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;

public class StringFill extends AFn {

  @Override
  public String getName() {
    return "string-fill!";
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

    Object c = args[1];
    if (!(c instanceof Character)) {
      throw new WrongTypeException("Character", c);
    }
    StringBuilder sb = new StringBuilder(s.length());
    for (int i = 0; i < s.length(); i++) {
      sb.append(c);
    }
    return sb.toString();
  }
}
