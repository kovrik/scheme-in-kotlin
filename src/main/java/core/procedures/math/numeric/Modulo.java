package core.procedures.math.numeric;

import core.exceptions.ArityException;
import core.procedures.AFn;

public class Modulo extends AFn implements INumericalOperation {

  // TODO move out
  private static final Remainder rem = new Remainder();

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

  public Number apply(Number first, Number second) {

    // check if they are integral
    if (!((first.doubleValue() == Math.floor(first.doubleValue())) &&
        !Double.isInfinite(first.doubleValue()))) {

      throw new IllegalArgumentException("Error: (modulo) bad argument type - not an integer: " + first);
    }
    if (!((second.doubleValue() == Math.floor(second.doubleValue())) &&
        !Double.isInfinite(second.doubleValue()))) {

      throw new IllegalArgumentException("Error: (modulo) bad argument type - not an integer: " + second);
    }

    Number m = rem.apply(first, second);
    if (m.intValue() == 0) {
      return m;
    }
    if ((first.intValue() > 0) == (second.intValue() > 0)) {
      return m;
    }
    if ((first instanceof Double) || (second instanceof Double)) {
      return m.doubleValue() + second.doubleValue();
    } else {
      return m.longValue() + second.longValue();
    }
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