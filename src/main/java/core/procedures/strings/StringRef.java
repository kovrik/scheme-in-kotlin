package core.procedures.strings;

import core.procedures.AFn;
import core.scm.FnArgs;

@FnArgs(args = {String.class, Long.class})
public class StringRef extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "string-ref";
  }

  @Override
  public Object invoke(Object... args) {
    String s = args[0].toString();
    Long pos = (Long)args[1];
    if ((pos < 0) || (pos >= s.length())) {
      throw new IllegalArgumentException(String.format("Value out of range: %s", pos));
    }
    return s.charAt(pos.intValue());
  }
}
