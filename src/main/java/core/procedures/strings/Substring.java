package core.procedures.strings;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;

public class Substring extends AFn {

  @Override
  public String getName() {
    return "substring";
  }

  @Override
  public String invoke(Object... args) {
    if (args.length < 2) {
      throw new ArityException(args.length, getName());
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
    long start = (long)p;
    if ((start < 0) || (start >= s.length())) {
      throw new IllegalArgumentException(String.format("Value out of range: %s", start));
    }
    long end = (long)s.length();

    if (args.length > 3) {
      throw new ArityException(args.length, getName());
    }
    Object oe = args[2];
    if (!(oe instanceof Long)) {
      throw new WrongTypeException("Integer", oe);
    }
    end = (long) oe;
    if ((end < 0) || (end >= s.length())) {
      throw new IllegalArgumentException(String.format("Value out of range: %s", end));
    }
    return s.substring((int)start, (int)end);
  }
}
