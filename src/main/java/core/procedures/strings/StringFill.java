package core.procedures.strings;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.MutableString;

public final class StringFill extends AFn {

  public StringFill() {
    super(new FnArgsBuilder().min(2).max(2)
                             .mandatory(new Class[]{MutableString.class, Character.class}).build());
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
  public MutableString apply2(Object arg1, Object arg2) {
    MutableString s = (MutableString)arg1;
    int oldLength = s.length();
    s.clear();
    for (int i = 0; i < oldLength; i++) {
      s.append(arg2);
    }
    return s;
  }
}
