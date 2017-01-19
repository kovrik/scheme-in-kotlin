package core.procedures.strings;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMClass;
import core.scm.SCMMutableString;
import core.scm.SCMUnspecified;

@FnArgs(minArgs = 3, maxArgs = 3, mandatoryArgsTypes = {SCMMutableString.class, SCMClass.ExactNonNegativeInteger.class, Character.class})
public final class StringSet extends AFn {

  @Override
  public String getName() {
    return "string-set!";
  }

  @Override
  public boolean isPure() {
    return false;
  }

  @Override
  public Object apply3(Object arg1, Object arg2, Object arg3) {
    SCMMutableString str = (SCMMutableString)arg1;
    Long pos = ((Number)arg2).longValue();
    if (pos >= str.length()) {
      throw new IllegalArgumentException(String.format("Value out of range: %s", pos));
    }
    str.setCharAt(pos.intValue(), (Character) arg3);
    return SCMUnspecified.UNSPECIFIED;
  }
}
