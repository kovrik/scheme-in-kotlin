package core.procedures.equivalence;

import core.scm.SCMBoolean;
import core.procedures.AFn;
import core.procedures.math.IOperation;

public class StringEq extends AFn implements IOperation {

  @Override
  public SCMBoolean invoke(Object... args) {
    Boolean result = zero();
    if (args != null && args.length > 1) {
      for (int i = 0; i < args.length - 1; i++) {
        result = result && apply(args[i], args[i + 1]);
      }
    }
    return SCMBoolean.toSCMBoolean(result);
  }

  public Boolean zero() {
    return Boolean.TRUE;
  }

  public Boolean apply(Object first, Object second) {
    if (!(first instanceof String) || !(second instanceof String)) {
      throw new IllegalArgumentException("Wrong type of argument to `string=?`");
    }
    return first.equals(second);
  }

  @Override
  public Object call() throws Exception {
    return invoke();
  }

  @Override
  public void run() {
    invoke();
  }
}
