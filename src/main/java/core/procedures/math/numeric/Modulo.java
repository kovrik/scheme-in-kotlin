package core.procedures.math.numeric;

import core.exceptions.ArityException;
import core.procedures.AFn;

public class Modulo extends AFn implements INumericalOperation {

  @Override
  public Number invoke(Object... args) {
    if (args != null && args.length == 2) {
      if (!(args[0] instanceof Number)) {
        throw new IllegalArgumentException("Wrong argument type. Expected: Integer, actual: " + args[0].getClass().getSimpleName());
      }
      if (!(args[1] instanceof Number)) {
        throw new IllegalArgumentException("Wrong argument type. Expected: Integer, actual: " + args[1].getClass().getSimpleName());
      }
      return apply((Number)args[0], (Number)args[1]);
    }
    throw new ArityException(args.length, 2, "modulo");
  }

  public Number zero() {
    throw new ArityException(0, 2, "modulo");
  }

  // FIXME
  public Number apply(Number first, Number second) {
    if ((first instanceof Double) || (second instanceof Double)) {
      double result = first.doubleValue() % second.doubleValue();
      // Don't want negative zero
      if (result == -0.0) {
        return Math.abs(result);
      }
      return result;
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
