package core.procedures.strings;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMClass;

@FnArgs(isVariadic = true, args = {String.class, SCMClass.ExactNonNegativeInteger.class})
public class Substring extends AFn {

  @Override
  public String getName() {
    return "substring";
  }

  @Override
  public String apply(Object... args) {
    String s = args[0].toString();
    long start = ((Number)args[1]).longValue();
    if (start >= s.length()) {
      throw new IllegalArgumentException(String.format("Value out of range: %s", start));
    }

    long end;
    if (args.length > 3) {
      throw new ArityException(args.length, getName());
    }
    Object oe = args[2];
    if (!(oe instanceof Long)) {
      throw new WrongTypeException(SCMClass.ExactNonNegativeInteger.class.getSimpleName(), oe);
    }
    end = (long) oe;
    if ((end < 0) || (end >= s.length())) {
      throw new IllegalArgumentException(String.format("Value out of range: %s", end));
    }
    return s.substring((int)start, (int)end);
  }
}
