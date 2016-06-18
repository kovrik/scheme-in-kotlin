package core.procedures.math.numeric;

import core.exceptions.ArityException;
import core.procedures.AFn;

public class Quotient extends AFn implements INumericalOperation {

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
    throw new ArityException(args.length, 2, "quotient");
  }

  public Number zero() {
    throw new ArityException(0, 2, "quotient");
  }

  public Number apply(Number first, Number second) {
    if ((first instanceof Double) || (second instanceof Double)) {
      // check if they are integral
      if (!((first.doubleValue() == Math.floor(first.doubleValue())) &&
            !Double.isInfinite(first.doubleValue()))) {

        throw new IllegalArgumentException("Error: (quotient) bad argument type - not an integer: " + first);
      }
      if (!((second.doubleValue() == Math.floor(second.doubleValue())) &&
          !Double.isInfinite(second.doubleValue()))) {

        throw new IllegalArgumentException("Error: (quotient) bad argument type - not an integer: " + second);
      }
      return (double)(first.longValue() / second.longValue());
    }
    return first.longValue() / second.longValue();
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
