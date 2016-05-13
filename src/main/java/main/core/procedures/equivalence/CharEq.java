package main.core.procedures.equivalence;

import main.core.ast.SCMBoolean;
import main.core.procedures.AFn;
import main.core.procedures.math.IOperation;

public class CharEq extends AFn implements IOperation {

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
    return Boolean.FALSE;
  }

  public Boolean apply(Object first, Object second) {
    if (!(first instanceof Character) || !(second instanceof Character)) {
      throw new IllegalArgumentException("Wrong type of argument to `char=?`");
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
