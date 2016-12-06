package core.procedures.strings;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMString;

@FnArgs(args = {String.class})
public class StringCopy extends AFn {

  @Override
  public String getName() {
    return "string-copy";
  }

  @Override
  public SCMString invoke(Object... args) {
    return new SCMString(args[0].toString());
  }
}
