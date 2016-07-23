package core.procedures.strings;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMBoolean;

import java.util.concurrent.ExecutionException;

public class StringComparison extends AFn {

  public static final StringComparison STRING_EQ = new StringComparison("string=?", new AFn() {
    @Override
    public Boolean invoke(Object arg1, Object arg2) {
      return arg1.equals(arg2);
    }
  });

  public static final StringComparison STRING_EQ_CI = new StringComparison("string-ci=?", new AFn() {
    @Override
    public Boolean invoke(Object arg1, Object arg2) {
      return ((String)arg1).equalsIgnoreCase((String)arg2);
    }
  });

  public static final StringComparison STRING_LE = new StringComparison("string<?", new AFn() {
    @Override
    public Boolean invoke(Object arg1, Object arg2) {
      return ((String)arg1).compareTo(((String)arg2)) < 0;
    }
  });

  public static final StringComparison STRING_LE_CI = new StringComparison("string-ci<?", new AFn() {
    @Override
    public Boolean invoke(Object arg1, Object arg2) {
      return ((String)arg1).toLowerCase().compareTo(((String)arg2).toLowerCase()) < 0;
    }
  });

  public static final StringComparison STRING_LE_OR_EQ = new StringComparison("string<=?", new AFn() {
    @Override
    public Boolean invoke(Object arg1, Object arg2) {
      return ((String)arg1).compareTo(((String)arg2)) <= 0;
    }
  });

  public static final StringComparison STRING_LE_OR_EQ_CI = new StringComparison("string-ci<=?", new AFn() {
    @Override
    public Boolean invoke(Object arg1, Object arg2) {
      return ((String)arg1).toLowerCase().compareTo(((String)arg2).toLowerCase()) <= 0;
    }
  });

  public static final StringComparison STRING_GR = new StringComparison("string>?", new AFn() {
    @Override
    public Boolean invoke(Object arg1, Object arg2) {
      return ((String)arg1).compareTo(((String)arg2)) > 0;
    }
  });

  public static final StringComparison STRING_GR_CI = new StringComparison("string-ci>?", new AFn() {
    @Override
    public Boolean invoke(Object arg1, Object arg2) {
      return ((String)arg1).toLowerCase().compareTo(((String)arg2).toLowerCase()) > 0;
    }
  });

  public static final StringComparison STRING_GR_OR_EQ = new StringComparison("string>=?", new AFn() {
    @Override
    public Boolean invoke(Object arg1, Object arg2) {
      return ((String)arg1).compareTo(((String)arg2)) >= 0;
    }
  });

  public static final StringComparison STRING_GR_OR_EQ_CI = new StringComparison("string-ci>=?", new AFn() {
    @Override
    public Boolean invoke(Object arg1, Object arg2) {
      return ((String)arg1).toLowerCase().compareTo(((String)arg2).toLowerCase()) >= 0;
    }
  });

  private final String name;
  private final AFn predicate;

  private StringComparison(String name, AFn predicate) {
    this.name = name;
    this.predicate = predicate;
  }

  @Override
  public Object invoke(Object... args) throws ExecutionException, InterruptedException {
    if (args.length < 2) {
      return SCMBoolean.TRUE;
    }
    Object f = args[0];
    if (!(f instanceof String)) {
      throw new WrongTypeException("String", f);
    }
    String prev = (String)f;
    for (int i = 1; i < args.length; i++) {
      Object arg = args[i];
      if (!(arg instanceof String)) {
        throw new WrongTypeException("String", arg);
      }
      if ((!(Boolean)predicate.invoke(prev, arg))) {
        return SCMBoolean.FALSE;
      }
      prev = (String)arg;
    }
    return SCMBoolean.TRUE;
  }

  @Override
  public String getName() {
    return name;
  }
}
