package core.procedures.strings;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.MutableString;
import core.scm.Type;
import core.scm.Void;

public final class StringSet extends AFn {

  public StringSet() {
    super(new FnArgsBuilder().min(3).max(3).mandatory(new Class[]{MutableString.class,
                                                                  Type.ExactNonNegativeInteger.class,
                                                                  Character.class}).build());
  }

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
    MutableString str = (MutableString)arg1;
    Long pos = ((Number)arg2).longValue();
    if (pos >= str.length()) {
      throw new IndexOutOfBoundsException(String.format("%s: value out of range: %s", getName(), pos));
    }
    str.setCharAt(pos.intValue(), (Character) arg3);
    return Void.VOID;
  }
}
