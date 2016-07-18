package core.procedures.equivalence;

import core.procedures.AFn;
import core.procedures.math.IOperation;
import core.scm.SCMBoolean;

public class Equal extends AFn implements IOperation {

  @Override
  public SCMBoolean invoke(Object... args) {
    Boolean result = zero();
    if (args.length > 1) {
      for (int i = 0; i < args.length - 1; i++) {
        result = result && apply(args[i], args[i + 1]);
      }
    }
    return SCMBoolean.toSCMBoolean(result);
  }

  @Override
  public SCMBoolean invoke(Object arg1, Object arg2) {
    return SCMBoolean.toSCMBoolean(equal(arg1, arg2));
  }

  public Boolean zero() {
    return Boolean.TRUE;
  }

  public Boolean apply(Object first, Object second) {
    return equal(first, second);
  }

  public static boolean equal(Object first, Object second) {
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
