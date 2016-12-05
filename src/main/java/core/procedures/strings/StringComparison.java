package core.procedures.strings;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMBoolean;
import core.scm.SCMString;

public class StringComparison extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  private static class StringComparisonFn extends AFn {
    public boolean invoke(Object arg1, Object arg2) {
      return (Boolean)super.invoke(arg1, arg2);
    }
  }

  public static final StringComparison STRING_EQ = new StringComparison("string=?", new StringComparisonFn() {
    @Override
    public boolean invoke(Object arg1, Object arg2) {
      return arg1.toString().equals(arg2.toString());
    }
  });

  public static final StringComparison STRING_EQ_CI = new StringComparison("string-ci=?", new StringComparisonFn() {
    @Override
    public boolean invoke(Object arg1, Object arg2) {
      return (arg1.toString()).equalsIgnoreCase(arg2.toString());
    }
  });

  public static final StringComparison STRING_LE = new StringComparison("string<?", new StringComparisonFn() {
    @Override
    public boolean invoke(Object arg1, Object arg2) {
      return (arg1.toString()).compareTo((arg2.toString())) < 0;
    }
  });

  public static final StringComparison STRING_LE_CI = new StringComparison("string-ci<?", new StringComparisonFn() {
    @Override
    public boolean invoke(Object arg1, Object arg2) {
      return (arg1.toString()).toLowerCase().compareTo((arg2.toString()).toLowerCase()) < 0;
    }
  });

  public static final StringComparison STRING_LE_OR_EQ = new StringComparison("string<=?", new StringComparisonFn() {
    @Override
    public boolean invoke(Object arg1, Object arg2) {
      return (arg1.toString()).compareTo((arg2.toString())) <= 0;
    }
  });

  public static final StringComparison STRING_LE_OR_EQ_CI = new StringComparison("string-ci<=?", new StringComparisonFn() {
    @Override
    public boolean invoke(Object arg1, Object arg2) {
      return (arg1.toString()).toLowerCase().compareTo((arg2.toString()).toLowerCase()) <= 0;
    }
  });

  public static final StringComparison STRING_GR = new StringComparison("string>?", new StringComparisonFn() {
    @Override
    public boolean invoke(Object arg1, Object arg2) {
      return (arg1.toString()).compareTo((arg2.toString())) > 0;
    }
  });

  public static final StringComparison STRING_GR_CI = new StringComparison("string-ci>?", new StringComparisonFn() {
    @Override
    public boolean invoke(Object arg1, Object arg2) {
      return (arg1.toString()).toLowerCase().compareTo((arg2.toString()).toLowerCase()) > 0;
    }
  });

  public static final StringComparison STRING_GR_OR_EQ = new StringComparison("string>=?", new StringComparisonFn() {
    @Override
    public boolean invoke(Object arg1, Object arg2) {
      return (arg1.toString()).compareTo((arg2.toString())) >= 0;
    }
  });

  public static final StringComparison STRING_GR_OR_EQ_CI = new StringComparison("string-ci>=?", new StringComparisonFn() {
    @Override
    public boolean invoke(Object arg1, Object arg2) {
      return (arg1.toString()).toLowerCase().compareTo((arg2.toString()).toLowerCase()) >= 0;
    }
  });

  private final String name;
  private final StringComparisonFn predicate;

  private StringComparison(String name, StringComparisonFn predicate) {
    this.name = name;
    this.predicate = predicate;
  }

  @Override
  public Object invoke(Object... args) {
    if (args.length < 2) {
      return SCMBoolean.TRUE;
    }
    for (int i = 0; i < args.length - 1; i++) {
      if (!(args[i] instanceof String || args[i] instanceof SCMString)) {
        throw new WrongTypeException("String", args[i]);
      }
      if (!(args[i + 1] instanceof String || args[i + 1] instanceof SCMString)) {
        throw new WrongTypeException("String", args[i + 1]);
      }
      if ((!predicate.invoke(args[i], args[i + 1]))) {
        return SCMBoolean.FALSE;
      }
    }
    return SCMBoolean.TRUE;
  }

  @Override
  public String getName() {
    return name;
  }
}
