package core.procedures.strings;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMMutableString;
import core.scm.SCMUnspecified;

@FnArgs(args = {SCMMutableString.class, Long.class, Character.class})
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
  public Object invoke(Object... args) {
    SCMMutableString str = (SCMMutableString)args[0];
    Long pos = ((Number)args[1]).longValue();
    if ((pos < 0) || (pos >= str.length())) {
      throw new IllegalArgumentException(String.format("Value out of range: %s", pos));
    }
    Object ch = args[2];
    str.setCharAt(pos.intValue(), (Character) ch);
    return SCMUnspecified.UNSPECIFIED;
  }
}
