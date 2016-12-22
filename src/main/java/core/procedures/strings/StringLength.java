package core.procedures.strings;

import core.procedures.AFn;
import core.scm.FnArgs;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {String.class})
public class StringLength extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "string-length";
  }

  @Override
  public Long apply(Object... args) {
    return ((Integer)(args[0].toString()).length()).longValue();
  }
}
