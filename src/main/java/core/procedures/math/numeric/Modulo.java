package core.procedures.math.numeric;

import core.exceptions.ArityException;
import core.procedures.AFn;

public class Modulo extends AFn implements INumericalOperation {

  @Override
  public Number invoke(Object... args) {
    if (args != null && args.length == 2) {
      return apply((Number)args[0], (Number)args[1]);
    }
    throw new ArityException(args.length, "modulo");
  }

  public Number zero() {
    throw new ArityException(0, "modulo");
  }

  public Number apply(Number first, Number second) {
    if ((first instanceof Double) || (second instanceof Double)) {
      return first.doubleValue() % second.doubleValue();
    } else if ((first instanceof Long) && (second instanceof Long)) {
      return (Long)first % (Long)second;
    }
    throw new IllegalArgumentException("Wrong type argument to `modulo`");
  }

  public Object apply(Object first, Object second) {
    return invoke(first, second);
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
