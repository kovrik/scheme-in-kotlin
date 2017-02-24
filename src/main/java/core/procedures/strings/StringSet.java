package core.procedures.strings;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMClass;
import core.scm.SCMMutableString;
import core.scm.SCMConstant;

public final class StringSet extends AFn {

  public StringSet() {
    super(new FnArgsBuilder().minArgs(3).maxArgs(3).mandatoryArgsTypes(new Class[]{SCMMutableString.class,
                                                                                   SCMClass.ExactNonNegativeInteger.class,
                                                                                   Character.class}));
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
    SCMMutableString str = (SCMMutableString)arg1;
    Long pos = ((Number)arg2).longValue();
    if (pos >= str.length()) {
      throw new IllegalArgumentException(String.format("%s: value out of range: %s", getName(), pos));
    }
    str.setCharAt(pos.intValue(), (Character) arg3);
    return SCMConstant.UNSPECIFIED;
  }
}
