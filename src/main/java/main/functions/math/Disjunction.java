package main.functions.math;

import main.functions.IFn;

public class Disjunction implements IBooleanOperation, IFn {

  public Object invoke(Object... args) {
    Boolean result = zero();
    if (args != null) {
      for (Object arg : args) {
        result = apply(result, (Boolean) arg);
        if (result) {
          return true;
        }
      }
    }
    return result;
  }

  public Boolean zero() {
    return Boolean.FALSE;
  }

  public Boolean apply(Boolean first, Boolean second) {
    return first || second;
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
