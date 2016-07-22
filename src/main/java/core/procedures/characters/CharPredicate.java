package core.procedures.characters;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.scm.SCMBoolean;
import core.scm.SCMCons;
import core.scm.SCMSymbol;

import java.util.List;
import java.util.concurrent.ExecutionException;

public class CharPredicate extends AFn {

  public static final CharPredicate CHAR_WHITESPACE = new CharPredicate("char-whitespace?", new AFn() {
    @Override
    public SCMBoolean invoke(Object ch) {
      return SCMBoolean.toSCMBoolean(Character.isWhitespace((Character)ch));
    }
  });

  public static final CharPredicate CHAR_ALPHABETIC = new CharPredicate("char-alphabetic?", new AFn() {
    @Override
    public SCMBoolean invoke(Object ch) {
      return SCMBoolean.toSCMBoolean(Character.isAlphabetic((Character)ch));
    }
  });

  public static final CharPredicate CHAR_UPPER_CASE = new CharPredicate("char-upper-case?", new AFn() {
    @Override
    public SCMBoolean invoke(Object ch) {
      return SCMBoolean.toSCMBoolean(Character.isUpperCase((Character)ch));
    }
  });

  public static final CharPredicate CHAR_LOWER_CASE = new CharPredicate("char-lower-case?", new AFn() {
    @Override
    public SCMBoolean invoke(Object ch) {
      return SCMBoolean.toSCMBoolean(Character.isLowerCase((Character)ch));
    }
  });

  public static final CharPredicate CHAR_NUMERIC = new CharPredicate("char-numeric?", new AFn() {
    @Override
    public SCMBoolean invoke(Object ch) {
      return SCMBoolean.toSCMBoolean(Character.isDigit((Character)ch));
    }
  });

  private static final SCMSymbol ch = new SCMSymbol("ch");
  private static final List<SCMSymbol> params = SCMCons.list(ch);

  private final String name;
  private final AFn predicate;

  public CharPredicate(String name, AFn predicate) {
    this.name = name;
    this.predicate = predicate;
  }

  @Override
  public SCMBoolean invoke(Object ch) {
    if (ch instanceof Character) {
      return (SCMBoolean)predicate.invoke(ch);
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
