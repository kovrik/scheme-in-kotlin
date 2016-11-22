package core.procedures.math;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMBigRational;
import core.scm.SCMBoolean;

import java.math.BigDecimal;

public class NumericalComparison extends AFn {

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

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return type.getSyntax();
  }

  private Type type;

  public NumericalComparison(Type type) {
    this.type = type;
  }

  @Override
  public SCMBoolean invoke(Object... args) {
    Boolean result = Boolean.TRUE;
    if (args != null && args.length > 1) {
      for (int i = 0; i < args.length - 1; i++) {
        result = result && invoke((Object)args[i], (Object)args[i + 1]);
      }
    }
    return SCMBoolean.toSCMBoolean(result);
  }

  public Boolean invoke(Object first, Object second) {
    if (!(first instanceof Number)) {
      throw new WrongTypeException("Number", first);
    }
    if (!(second instanceof Number)) {
      throw new WrongTypeException("Number", second);
    }

    Number f = (Number)first;
    Number s = (Number)second;
    if (f instanceof SCMBigRational) {
      f = ((SCMBigRational)f).toBigDecimal();
    }
    if (s instanceof SCMBigRational) {
      s = ((SCMBigRational)s).toBigDecimal();
    }
    if ((f instanceof Double) || (s instanceof Double)) {
      f = f.doubleValue();
      s = s.doubleValue();
    } else if ((f instanceof BigDecimal) || (s instanceof BigDecimal)) {
      f = new BigDecimal(f.toString());
      s = new BigDecimal(s.toString());
    }
    switch (type) {
      case EQUAL:         return ((Comparable)f).compareTo(s) == 0;
      case LESS:          return ((Comparable)f).compareTo(s) < 0;
      case GREATER:       return ((Comparable)f).compareTo(s) > 0;
      case LESS_EQUAL:    return ((Comparable)f).compareTo(s) <= 0;
      case GREATER_EQUAL: return ((Comparable)f).compareTo(s) >= 0;
    }
    throw new IllegalArgumentException("Unknown comparison type!");
  }
}
