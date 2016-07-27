package core.procedures.equivalence;

import core.procedures.AFn;
import core.scm.SCMBoolean;

public class Equal extends AFn {

  @Override
  public String getName() {
    return "equal?";
  }

  @Override
  public SCMBoolean invoke(Object... args) {
    Boolean result = Boolean.TRUE;
    if (args.length > 1) {
      for (int i = 0; i < args.length - 1; i++) {
        result = result && equal(args[i], args[i + 1]);
      }
    }
    return SCMBoolean.toSCMBoolean(result);
  }

  public static boolean equal(Object first, Object second) {
    return first.equals(second);
  }
}
