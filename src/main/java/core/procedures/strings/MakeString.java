package core.procedures.strings;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;

public class MakeString extends AFn {

  @Override
  public String getName() {
    return "make-string";
  }

  @Override
  public String invoke(Object... args) {
    if (args.length < 1) {
      throw new ArityException(args.length, getName());
    }

    Object o = args[0];
    if (!(o instanceof Long)) {
      throw new WrongTypeException("Integer", o);
    }
    Long s = (Long)o;
    if (s < 0) {
      throw new IllegalArgumentException(String.format("Size value is out of range in `%s`", getName()));
    }
    if (args.length > 2) {
      throw new ArityException(args.length, getName());
    }
    Object c = (args.length == 1) ? Character.MIN_VALUE : args[1];
    if (!(c instanceof Character)) {
      throw new WrongTypeException("Character", c);
    }
    StringBuilder sb = new StringBuilder();
    for (long i = 0; i < s; i++) {
      sb.append(c);
    }
    return sb.toString();
  }
}
