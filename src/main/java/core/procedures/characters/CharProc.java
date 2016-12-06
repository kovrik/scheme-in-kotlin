package core.procedures.characters;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBoolean;

@FnArgs(args = {Character.class})
public class CharProc extends AFn {

  private static class CharFn extends AFn {
    public Object invoke(Object ch) {
      return super.invoke(ch);
    }
  }

  public static final CharProc CHAR_WHITESPACE = new CharProc("char-whitespace?", new CharFn() {
    @Override
    public SCMBoolean invoke(Object ch) {
      return SCMBoolean.toSCMBoolean(Character.isWhitespace((Character)ch));
    }
  });

  public static final CharProc CHAR_ALPHABETIC = new CharProc("char-alphabetic?", new CharFn() {
    @Override
    public SCMBoolean invoke(Object ch) {
      return SCMBoolean.toSCMBoolean(Character.isAlphabetic((Character)ch));
    }
  });

  public static final CharProc CHAR_UPPER_CASE = new CharProc("char-upper-case?", new CharFn() {
    @Override
    public SCMBoolean invoke(Object ch) {
      return SCMBoolean.toSCMBoolean(Character.isUpperCase((Character)ch));
    }
  });

  public static final CharProc CHAR_LOWER_CASE = new CharProc("char-lower-case?", new CharFn() {
    @Override
    public SCMBoolean invoke(Object ch) {
      return SCMBoolean.toSCMBoolean(Character.isLowerCase((Character)ch));
    }
  });

  public static final CharProc CHAR_NUMERIC = new CharProc("char-numeric?", new CharFn() {
    @Override
    public SCMBoolean invoke(Object ch) {
      return SCMBoolean.toSCMBoolean(Character.isDigit((Character)ch));
    }
  });

  public static final CharProc CHAR_TO_INTEGER = new CharProc("char->integer", new CharFn() {
    @Override
    public Long invoke(Object ch) {
      return (long)((char)ch);
    }
  });

  public static final CharProc INTEGER_TO_CHAR = new CharProc("integer->char", new CharFn() {
    @Override
    public Character invoke(Object ch) {
      return (char) ((long) ch);
    }
  }) {
    @Override
    public Object invoke(Object n) {
      if (n instanceof Long) {
        return this.predicate.invoke(n);
      }
      throw new WrongTypeException("Integer", n);
    }
  };

  public static final CharProc CHAR_UPCASE = new CharProc("char-upcase", new CharFn() {
    @Override
    public Character invoke(Object ch) {
      return Character.toUpperCase((Character)ch);
    }
  });

  public static final CharProc CHAR_DOWNCASE = new CharProc("char-downcase", new CharFn() {
    @Override
    public Character invoke(Object ch) {
      return Character.toLowerCase((Character)ch);
    }
  });

  private final String name;
  protected final CharFn predicate;

  private CharProc(String name, CharFn predicate) {
    this.name = name;
    this.predicate = predicate;
  }

  public Object invoke(Object ch) {
    return predicate.invoke(ch);
  }

  @Override
  public Object invoke(Object... args) {
    return invoke(args[0]);
  }

  @Override
  public String getName() {
    return name;
  }
}
