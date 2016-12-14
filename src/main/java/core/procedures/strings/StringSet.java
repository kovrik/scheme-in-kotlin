package core.procedures.strings;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMClass;
import core.scm.SCMMutableString;
import core.scm.SCMUnspecified;

@FnArgs(args = {SCMMutableString.class, SCMClass.ExactNonNegativeInteger.class, Character.class})
public class StringSet extends AFn {

  @Override
  public String getName() {
    return "string-set!";
  }

  @Override
  public boolean isPure() {
    return false;
  }

  @Override
  public Object apply(Object... args) {
    SCMMutableString str = (SCMMutableString)args[0];
    Long pos = ((Number)args[1]).longValue();
    if (pos >= str.length()) {
      throw new IllegalArgumentException(String.format("Value out of range: %s", pos));
    }
    Object ch = args[2];
    str.setCharAt(pos.intValue(), (Character) ch);
    return SCMUnspecified.UNSPECIFIED;
  }
}
