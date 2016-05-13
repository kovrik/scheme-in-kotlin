package main.core.procedures.math.numeric;

import main.core.procedures.AFn;
import main.core.procedures.math.IOperation;

public class NumericalComparison extends AFn implements IOperation {

  public enum Type {
    EQUAL, LESS, GREATER, LESS_EQUAL, GREATER_EQUAL;
  }

  private Type type;

  public NumericalComparison(Type type) {
    this.type = type;
  }

  @Override
  public Boolean invoke(Object... args) {
    Boolean result = zero();
    if (args != null && args.length > 1) {
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
    switch (type) {
      case EQUAL: {
        return ((Comparable)first).compareTo((Comparable) second) == 0;
      }
      case LESS: {
        return ((Comparable)first).compareTo((Comparable) second) < 0;
      }
      case GREATER: {
        return ((Comparable)first).compareTo((Comparable) second) > 0;
      }
      case LESS_EQUAL: {
        return ((Comparable)first).compareTo((Comparable) second) <= 0;
      }
      case GREATER_EQUAL: {
        return ((Comparable)first).compareTo((Comparable) second) <= 0;
      }
    }
    throw new IllegalArgumentException("Unknown comparison type!");
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
