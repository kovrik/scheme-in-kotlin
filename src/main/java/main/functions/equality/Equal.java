package main.functions.equality;

import main.functions.IFn;
import main.functions.math.IOperation;

public class Equal implements IOperation, IFn {

  public Boolean invoke(Object... args) {
    Boolean result = zero();
    if (args.length > 1) {
      for (int i = 0; i < args.length - 1; i++) {
        result = result && apply(args[i], args[i + 1]);
      }
    }
    return result;
  }

  public Boolean zero() {
    return Boolean.TRUE;
  }

  public Boolean apply(Object first, Object second) {

    return first.equals(second);
  }

  public Object call() throws Exception {
    return invoke();
  }

  public void run() {
    invoke();
  }
}
