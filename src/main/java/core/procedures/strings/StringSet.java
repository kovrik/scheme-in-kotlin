package core.procedures.strings;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMString;
import core.scm.SCMUnspecified;

@FnArgs(args = {String.class, Long.class, Character.class})
public class StringSet extends AFn {

  @Override
  public String getName() {
    return "string-set!";
  }

  @Override
  public Object invoke(Object... args) {
    SCMString str = (SCMString)args[0];
    Long pos = (Long)args[1];
    if ((pos < 0) || (pos >= str.length())) {
      throw new IllegalArgumentException(String.format("Value out of range: %s", pos));
    }
    Object ch = args[2];
    str.setCharAt(pos.intValue(), (Character) ch);
    return SCMUnspecified.UNSPECIFIED;
  }
}
