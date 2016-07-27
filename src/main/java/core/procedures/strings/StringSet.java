package core.procedures.strings;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;

public class StringSet extends AFn {

  @Override
  public String getName() {
    return "string-set!";
  }

  @Override
  public Object invoke(Object... args) {
    if (args.length != 3) {
      throw new ArityException(args.length, 3, getName());
    }

    Object o = args[0];
    if (!(o instanceof String)) {
      throw new WrongTypeException("String", o);
    }
    String str = (String)o;

    Object p = args[1];
    if (!(p instanceof Long)) {
      throw new WrongTypeException("Integer", p);
    }
    Long pos = (Long)p;
    if ((pos < 0) || (pos >= str.length())) {
      throw new IllegalArgumentException(String.format("Value out of range: %s", pos));
    }
    Object ch = args[2];
    if (!(ch instanceof Character)) {
      throw new WrongTypeException("Character", ch);
    }
    Character character = (Character) ch;
    String before = str.substring(0, pos.intValue());
    String after = str.substring(pos.intValue() + 1, str.length());
    return before + character + after;
    // FIXME Make modifiable string!
//    return SCMSpecialForm.UNSPECIFIED;
  }
}
