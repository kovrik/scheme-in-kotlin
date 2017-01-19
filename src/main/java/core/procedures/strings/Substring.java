package core.procedures.strings;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMClass;

@FnArgs(minArgs = 2, maxArgs = 3,
  mandatoryArgsTypes = {String.class, SCMClass.ExactNonNegativeInteger.class},
  restArgsType = SCMClass.ExactNonNegativeInteger.class)
public final class Substring extends AFn {

  @Override
  public String getName() {
    return "substring";
  }

  @Override
  public String apply(Object... args) {
    String s = args[0].toString();
    long start = ((Number)args[1]).longValue();
    if (start > s.length()) {
      throw new IllegalArgumentException(String.format("Value out of range: %s", start));
    }

    long end = s.length();
    if (args.length == 3) {
      end = (long) args[2];
    }
    if (end > s.length()) {
      throw new IllegalArgumentException(String.format("Value out of range: %s", end));
    }
    return s.substring((int)start, (int)end);
  }
}
