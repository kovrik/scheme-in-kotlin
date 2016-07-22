package core.procedures.characters;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMBoolean;
import core.scm.SCMCons;
import core.scm.SCMSymbol;

import java.util.List;
import java.util.concurrent.ExecutionException;

public class CharComparison extends AFn {

  public static final CharComparison CHAR_LE = new CharComparison("char<?", new AFn() {
    @Override
    public Boolean invoke(Object arg1, Object arg2) {
      return ((Character)arg1).compareTo((Character)arg2) < 0;
    }
  });

  public static final CharComparison CHAR_LE_CI = new CharComparison("char-ci<?", new AFn() {
    @Override
    public Boolean invoke(Object arg1, Object arg2) {
      return ((Character.toLowerCase((Character)arg1))) < ((Character.toLowerCase((Character)arg2)));
    }
  });

  public static final CharComparison CHAR_LE_OR_EQ = new CharComparison("char<=?", new AFn() {
    @Override
    public Boolean invoke(Object arg1, Object arg2) {
      return ((Character)arg1).compareTo((Character)arg2) <= 0;
    }
  });

  public static final CharComparison CHAR_LE_OR_EQ_CI = new CharComparison("char-ci<=?", new AFn() {
    @Override
    public Boolean invoke(Object arg1, Object arg2) {
      return ((Character.toLowerCase((Character)arg1))) <= ((Character.toLowerCase((Character)arg2)));
    }
  });

  public static final CharComparison CHAR_GR = new CharComparison("char>?", new AFn() {
    @Override
    public Boolean invoke(Object arg1, Object arg2) {
      return ((Character)arg1).compareTo((Character)arg2) > 0;
    }
  });

  public static final CharComparison CHAR_GR_CI = new CharComparison("char-ci>?", new AFn() {
    @Override
    public Boolean invoke(Object arg1, Object arg2) {
      return ((Character.toLowerCase((Character)arg1))) > ((Character.toLowerCase((Character)arg2)));
    }
  });

  public static final CharComparison CHAR_GR_OR_EQ = new CharComparison("char>=?", new AFn() {
    @Override
    public Boolean invoke(Object arg1, Object arg2) {
      return ((Character)arg1).compareTo((Character)arg2) >= 0;
    }
  });

  public static final CharComparison CHAR_GR_OR_EQ_CI = new CharComparison("char-ci>=?", new AFn() {
    @Override
    public Boolean invoke(Object arg1, Object arg2) {
      return ((Character.toLowerCase((Character)arg1))) >= ((Character.toLowerCase((Character)arg2)));
    }
  });

  private static final SCMSymbol ch = new SCMSymbol("ch");
  private static final List<SCMSymbol> params = SCMCons.list(ch);

  private final String name;
  private final AFn predicate;

  private CharComparison(String name, AFn predicate) {
    this.name = name;
    this.predicate = predicate;
  }

  @Override
  public Object invoke(Object... args) throws ExecutionException, InterruptedException {
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
      if ((!(Boolean)predicate.invoke(prev, arg))) {
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
