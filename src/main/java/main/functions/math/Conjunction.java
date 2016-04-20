package main.functions.math;

import main.functions.IFn;

public class Conjunction implements IBooleanOperation, IFn {

  public Object invoke(Object... args) {
    Boolean result = zero();
    if (args != null) {
      for (Object arg : args) {
        result = apply(result, (Boolean) arg);
        if (!result) {
          return result;
        }
      }
    }
    return result;
  }

  public Boolean zero() {
    return Boolean.TRUE;
  }

  public Boolean apply(Boolean first, Boolean second) {
    return first && second;
  }

  public Object apply(Object first, Object second) {
    return apply((Boolean) first, (Boolean) second);
  }

  public Object call() throws Exception {
    return invoke();
  }

  public void run() {
    invoke();
  }
}
