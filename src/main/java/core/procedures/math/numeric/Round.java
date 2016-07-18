package core.procedures.math.numeric;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.writer.Writer;

import java.math.BigDecimal;
import java.math.MathContext;

public class Round extends AFn implements INumericalOperation {

  @Override
  public Number invoke(Object... args) {
    if (args != null && args.length == 1) {
      if (args[0] instanceof Long) {
        return (Long)args[0];
      } else if (args[0] instanceof Double) {
        return Math.rint((Double)args[0]);
      } else if (args[0] instanceof BigDecimal) {
        return ((BigDecimal)args[0]).round(MathContext.DECIMAL32);
      }
      throw new IllegalArgumentException("Wrong argument type. Expected: Number, actual: " + Writer.write(args[0]));
    }
    throw new ArityException(args.length, 1, "round");
  }

  @Override
  public Number zero() {
    throw new ArityException(0, 1, "round");
  }

  @Override
  public Number apply(Number first, Number second) {
    throw new ArityException(2, 1, "round");
  }

  @Override
  public Object apply(Object first, Object second) {
    throw new ArityException(2, 1, "round");
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
