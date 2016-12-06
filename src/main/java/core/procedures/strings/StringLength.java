package core.procedures.strings;

import core.procedures.AFn;
import core.scm.FnArgs;

@FnArgs(args = {String.class})
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
  public Long invoke(Object... args) {
    return ((Integer)(args[0].toString()).length()).longValue();
  }
}
