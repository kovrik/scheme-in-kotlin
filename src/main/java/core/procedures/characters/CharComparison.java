package core.procedures.characters;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBoolean;

@FnArgs(isVariadic = true)
public class CharComparison extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  private static class CharComparisonFn extends AFn {
    public boolean apply(Object arg1, Object arg2) {
      return (Boolean)super.apply(arg1, arg2);
    }
  }

  public static final CharComparison CHAR_EQ = new CharComparison("char=?", new CharComparisonFn() {
    @Override
    public boolean apply(Object arg1, Object arg2) {
      return ((Character)arg1).compareTo((Character)arg2) == 0;
    }
  });

  public static final CharComparison CHAR_EQ_CI = new CharComparison("char-ci=?", new CharComparisonFn() {
    @Override
    public boolean apply(Object arg1, Object arg2) {
      return ((Character.toLowerCase((Character)arg1))) == ((Character.toLowerCase((Character)arg2)));
    }
  });

  public static final CharComparison CHAR_LE = new CharComparison("char<?", new CharComparisonFn() {
    @Override
    public boolean apply(Object arg1, Object arg2) {
      return ((Character)arg1).compareTo((Character)arg2) < 0;
    }
  });

  public static final CharComparison CHAR_LE_CI = new CharComparison("char-ci<?", new CharComparisonFn() {
    @Override
    public boolean apply(Object arg1, Object arg2) {
      return ((Character.toLowerCase((Character)arg1))) < ((Character.toLowerCase((Character)arg2)));
    }
  });

  public static final CharComparison CHAR_LE_OR_EQ = new CharComparison("char<=?", new CharComparisonFn() {
    @Override
    public boolean apply(Object arg1, Object arg2) {
      return ((Character)arg1).compareTo((Character)arg2) <= 0;
    }
  });

  public static final CharComparison CHAR_LE_OR_EQ_CI = new CharComparison("char-ci<=?", new CharComparisonFn() {
    @Override
    public boolean apply(Object arg1, Object arg2) {
      return ((Character.toLowerCase((Character)arg1))) <= ((Character.toLowerCase((Character)arg2)));
    }
  });

  public static final CharComparison CHAR_GR = new CharComparison("char>?", new CharComparisonFn() {
    @Override
    public boolean apply(Object arg1, Object arg2) {
      return ((Character)arg1).compareTo((Character)arg2) > 0;
    }
  });

  public static final CharComparison CHAR_GR_CI = new CharComparison("char-ci>?", new CharComparisonFn() {
    @Override
    public boolean apply(Object arg1, Object arg2) {
      return ((Character.toLowerCase((Character)arg1))) > ((Character.toLowerCase((Character)arg2)));
    }
  });

  public static final CharComparison CHAR_GR_OR_EQ = new CharComparison("char>=?", new CharComparisonFn() {
    @Override
    public boolean apply(Object arg1, Object arg2) {
      return ((Character)arg1).compareTo((Character)arg2) >= 0;
    }
  });

  public static final CharComparison CHAR_GR_OR_EQ_CI = new CharComparison("char-ci>=?", new CharComparisonFn() {
    @Override
    public boolean apply(Object arg1, Object arg2) {
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
  public Object apply(Object... args) {
    if (args.length < 2) {
      return SCMBoolean.TRUE;
    }
    for (int i = 0; i < args.length - 1; i++) {
      if (!(args[i] instanceof Character)) {
        throw new WrongTypeException("Character", args[i]);
      }
      if (!(args[i + 1] instanceof Character)) {
        throw new WrongTypeException("Character", args[i + 1]);
      }
      if ((!predicate.apply(args[i], args[i + 1]))) {
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
