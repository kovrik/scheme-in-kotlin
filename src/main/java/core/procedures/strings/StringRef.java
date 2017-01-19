package core.procedures.strings;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMClass;

@FnArgs(minArgs = 2, maxArgs = 2, mandatoryArgsTypes = {String.class, SCMClass.ExactNonNegativeInteger.class})
public final class StringRef extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "string-ref";
  }

  @Override
  public Object apply2(Object arg1, Object arg2) {
    String s = arg1.toString();
    Long pos = ((Number)arg2).longValue();
    if (pos >= s.length()) {
      throw new IllegalArgumentException(String.format("Value out of range: %s", pos));
    }
    return s.charAt(pos.intValue());
  }
}
