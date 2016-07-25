package core.procedures.characters;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMBoolean;

public class CharComparison extends AFn {

  private static class CharComparisonFn extends AFn {
    public Boolean invoke(Object arg1, Object arg2) {
      return (Boolean)super.invoke(arg1, arg2);
    }
  }

  public static final CharComparison CHAR_EQ = new CharComparison("char=?", new CharComparisonFn() {
    @Override
    public Boolean invoke(Object arg1, Object arg2) {
      return ((Character)arg1).compareTo((Character)arg2) == 0;
    }
  });

  public static final CharComparison CHAR_EQ_CI = new CharComparison("char-ci=?", new CharComparisonFn() {
    @Override
    public Boolean invoke(Object arg1, Object arg2) {
      return ((Character.toLowerCase((Character)arg1))) == ((Character.toLowerCase((Character)arg2)));
    }
  });

  public static final CharComparison CHAR_LE = new CharComparison("char<?", new CharComparisonFn() {
    @Override
    public Boolean invoke(Object arg1, Object arg2) {
      return ((Character)arg1).compareTo((Character)arg2) < 0;
    }
  });

  public static final CharComparison CHAR_LE_CI = new CharComparison("char-ci<?", new CharComparisonFn() {
    @Override
    public Boolean invoke(Object arg1, Object arg2) {
      return ((Character.toLowerCase((Character)arg1))) < ((Character.toLowerCase((Character)arg2)));
    }
  });

  public static final CharComparison CHAR_LE_OR_EQ = new CharComparison("char<=?", new CharComparisonFn() {
    @Override
    public Boolean invoke(Object arg1, Object arg2) {
      return ((Character)arg1).compareTo((Character)arg2) <= 0;
    }
  });

  public static final CharComparison CHAR_LE_OR_EQ_CI = new CharComparison("char-ci<=?", new CharComparisonFn() {
    @Override
    public Boolean invoke(Object arg1, Object arg2) {
      return ((Character.toLowerCase((Character)arg1))) <= ((Character.toLowerCase((Character)arg2)));
    }
  });

  public static final CharComparison CHAR_GR = new CharComparison("char>?", new CharComparisonFn() {
    @Override
    public Boolean invoke(Object arg1, Object arg2) {
      return ((Character)arg1).compareTo((Character)arg2) > 0;
    }
  });

  public static final CharComparison CHAR_GR_CI = new CharComparison("char-ci>?", new CharComparisonFn() {
    @Override
    public Boolean invoke(Object arg1, Object arg2) {
      return ((Character.toLowerCase((Character)arg1))) > ((Character.toLowerCase((Character)arg2)));
    }
  });

  public static final CharComparison CHAR_GR_OR_EQ = new CharComparison("char>=?", new CharComparisonFn() {
    @Override
    public Boolean invoke(Object arg1, Object arg2) {
      return ((Character)arg1).compareTo((Character)arg2) >= 0;
    }
  });

  public static final CharComparison CHAR_GR_OR_EQ_CI = new CharComparison("char-ci>=?", new CharComparisonFn() {
    @Override
    public Boolean invoke(Object arg1, Object arg2) {
      return ((Character.toLowerCase((Character)arg1))) >= ((Character.toLowerCase((Character)arg2)));
    }
  });

  private final String name;
  private final CharComparisonFn predicate;

  private CharComparison(String name, CharComparisonFn predicate) {
    this.name = name;
    this.predicate = predicate;
  }

  @Override
  public Object invoke(Object... args) {
    if (args.length < 2) {
      return SCMBoolean.TRUE;
    }
    Object f = args[0];
    if (!(f instanceof Character)) {
      throw new WrongTypeException("Character", f);
    }
    char prev = (Character)f;
    for (int i = 1; i < args.length; i++) {
      Object arg = args[i];
      if (!(arg instanceof Character)) {
        throw new WrongTypeException("Character", arg);
      }
      if ((!predicate.invoke(prev, arg))) {
        return SCMBoolean.FALSE;
      }
      prev = (char)arg;
    }
    return SCMBoolean.TRUE;
  }

  @Override
  public String getName() {
    return name;
  }
}
