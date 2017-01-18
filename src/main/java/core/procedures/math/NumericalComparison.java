package core.procedures.math;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBigRational;
import core.scm.SCMClass;

import java.math.BigDecimal;

@FnArgs(minArgs = 2, mandatoryArgsTypes = {SCMClass.Real.class, SCMClass.Real.class}, restArgsType = SCMClass.Real.class)
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
  public Boolean apply(Object... args) {
    for (int i = 0; i < args.length - 1; i++) {
      if (!apply(args[i], args[i + 1], type)) {
        return Boolean.FALSE;
      }
    }
    return Boolean.TRUE;
  }

  public static boolean apply(Object first, Object second, Type type) {
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
      case LESS:          return ((Comparable)f).compareTo(s) <  0;
      case GREATER:       return ((Comparable)f).compareTo(s) >  0;
      case LESS_EQUAL:    return ((Comparable)f).compareTo(s) <= 0;
      case GREATER_EQUAL: return ((Comparable)f).compareTo(s) >= 0;
      default: throw new IllegalArgumentException("Unknown comparison type!");
    }
  }
}
