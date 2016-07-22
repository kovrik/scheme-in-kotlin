package core.procedures.characters;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMBoolean;

import java.util.concurrent.ExecutionException;

public class CharProc extends AFn {

  public static final CharProc CHAR_WHITESPACE = new CharProc("char-whitespace?", new AFn() {
    @Override
    public SCMBoolean invoke(Object ch) {
      return SCMBoolean.toSCMBoolean(Character.isWhitespace((Character)ch));
    }
  });

  public static final CharProc CHAR_ALPHABETIC = new CharProc("char-alphabetic?", new AFn() {
    @Override
    public SCMBoolean invoke(Object ch) {
      return SCMBoolean.toSCMBoolean(Character.isAlphabetic((Character)ch));
    }
  });

  public static final CharProc CHAR_UPPER_CASE = new CharProc("char-upper-case?", new AFn() {
    @Override
    public SCMBoolean invoke(Object ch) {
      return SCMBoolean.toSCMBoolean(Character.isUpperCase((Character)ch));
    }
  });

  public static final CharProc CHAR_LOWER_CASE = new CharProc("char-lower-case?", new AFn() {
    @Override
    public SCMBoolean invoke(Object ch) {
      return SCMBoolean.toSCMBoolean(Character.isLowerCase((Character)ch));
    }
  });

  public static final CharProc CHAR_NUMERIC = new CharProc("char-numeric?", new AFn() {
    @Override
    public SCMBoolean invoke(Object ch) {
      return SCMBoolean.toSCMBoolean(Character.isDigit((Character)ch));
    }
  });

  public static final CharProc CHAR_TO_INTEGER = new CharProc("char->integer", new AFn() {
    @Override
    public Long invoke(Object ch) {
      return (long)((char)ch);
    }
  });

  public static final CharProc INTEGER_TO_CHAR = new CharProc("integer->char", new AFn() {
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

  public static final CharProc CHAR_UPCASE = new CharProc("char-upcase", new AFn() {
    @Override
    public Character invoke(Object ch) {
      return Character.toUpperCase((Character)ch);
    }
  });

  public static final CharProc CHAR_DOWNCASE = new CharProc("char-downcase", new AFn() {
    @Override
    public Character invoke(Object ch) {
      return Character.toLowerCase((Character)ch);
    }
  });

  private final String name;
  protected final AFn predicate;

  private CharProc(String name, AFn predicate) {
    this.name = name;
    this.predicate = predicate;
  }

  @Override
  public Object invoke(Object ch) {
    if (ch instanceof Character) {
      return predicate.invoke(ch);
    }
    throw new WrongTypeException("Character", ch);
  }

  @Override
  public Object invoke(Object... args) throws ExecutionException, InterruptedException {
    if (args.length == 1) {
      return invoke(args[0]);
    }
    throw new ArityException(args.length, 1, getName());
  }

  @Override
  public String getName() {
    return name;
  }
}
