package core.procedures.strings;

import core.procedures.AFn;
import core.scm.FnArgs;

@FnArgs(args = {String.class, Character.class})
public class StringFill extends AFn {

  @Override
  public String getName() {
    return "string-fill!";
  }

  @Override
  public Object invoke(Object... args) {
    String s = args[0].toString();
    Object c = args[1];
    StringBuilder sb = new StringBuilder(s.length());
    for (int i = 0; i < s.length(); i++) {
      sb.append(c);
    }
    return sb.toString();
  }
}
