package core.procedures.math.numeric;

import core.procedures.AFn;
import core.procedures.math.IOperation;
import core.scm.SCMBoolean;

public class NumericalComparison extends AFn implements IOperation {

  public enum Type {
    EQUAL("="),
    LESS("<"),
    GREATER(">"),
    LESS_EQUAL("<="),
    GREATER_EQUAL(">=");

    private String syntax;

    Type(String syntax) {
      this.syntax = syntax;
    }

    public String getSyntax() {
      return syntax;
    }
  }

  private Type type;

  public NumericalComparison(Type type) {
    this.type = type;
  }

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
    return Boolean.TRUE;
  }

  public Boolean apply(Object first, Object second) {

    if (!(first instanceof Number)) {
      throw new IllegalArgumentException("Wrong argument type. Expected: Number, actual: " + first.getClass().getSimpleName());
    }
    if (!(second instanceof Number)) {
      throw new IllegalArgumentException("Wrong argument type. Expected: Number, actual: " + second.getClass().getSimpleName());
    }

    Number f = (Number)first;
    Number s = (Number)second;
    if ((first instanceof Double) || (second instanceof Double)) {
      f = f.doubleValue();
      s = s.doubleValue();
    }

    switch (type) {
      case EQUAL: {
        return ((Comparable)f).compareTo(s) == 0;
      }
      case LESS: {
        return ((Comparable)f).compareTo(s) < 0;
      }
      case GREATER: {
        return ((Comparable)f).compareTo(s) > 0;
      }
      case LESS_EQUAL: {
        return ((Comparable)f).compareTo(s) <= 0;
      }
      case GREATER_EQUAL: {
        return ((Comparable)f).compareTo(s) >= 0;
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
