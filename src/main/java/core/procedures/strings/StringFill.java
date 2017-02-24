package core.procedures.strings;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMMutableString;

public final class StringFill extends AFn {

  public StringFill() {
    super(new FnArgsBuilder().minArgs(2).maxArgs(2)
                             .mandatoryArgsTypes(new Class[]{SCMMutableString.class, Character.class}));
  }

  @Override
  public String getName() {
    return "string-fill!";
  }

  @Override
  public boolean isPure() {
    return false;
  }

  @Override
  public SCMMutableString apply2(Object arg1, Object arg2) {
    SCMMutableString s = (SCMMutableString)arg1;
    int oldLength = s.length();
    s.clear();
    for (int i = 0; i < oldLength; i++) {
      s.append(arg2);
    }
    return s;
  }
}
