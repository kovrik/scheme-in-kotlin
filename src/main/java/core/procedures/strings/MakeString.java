package core.procedures.strings;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMClass;
import core.scm.SCMMutableString;

@FnArgs(minArgs = 1, maxArgs = 2, mandatoryArgsTypes = {SCMClass.ExactNonNegativeInteger.class}, restArgsType = {Character.class})
public final class MakeString extends AFn {

  @Override
  public String getName() {
    return "make-string";
  }

  @Override
  public SCMMutableString apply(Object... args) {
    Long s = ((Number)args[0]).longValue();
    Object c = (args.length == 1) ? Character.MIN_VALUE : args[1];
    SCMMutableString string = new SCMMutableString();
    for (long i = 0; i < s; i++) {
      string.append(c);
    }
    return string;
  }
}
