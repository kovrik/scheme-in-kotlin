package core.procedures.strings;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMMutableString;

@FnArgs(minArgs = 2, maxArgs = 2, mandatoryArgsTypes = {SCMMutableString.class, Character.class})
public class StringFill extends AFn {

  @Override
  public String getName() {
    return "string-fill!";
  }

  @Override
  public boolean isPure() {
    return false;
  }

  @Override
  public SCMMutableString apply(Object... args) {
    SCMMutableString s = (SCMMutableString)args[0];
    int oldLength = s.length();
    Object c = args[1];
    s.clear();
    for (int i = 0; i < oldLength; i++) {
      s.append(c);
    }
    return s;
  }
}
