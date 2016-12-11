package core.procedures.strings;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.FnArgs;

@FnArgs(isVariadic = true, args = {String.class, Long.class})
public class Substring extends AFn {

  @Override
  public String getName() {
    return "substring";
  }

  @Override
  public String invoke(Object... args) {
    String s = args[0].toString();
    long start = ((Number)args[1]).longValue();
    if ((start < 0) || (start >= s.length())) {
      throw new IllegalArgumentException(String.format("Value out of range: %s", start));
    }

    long end;
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
