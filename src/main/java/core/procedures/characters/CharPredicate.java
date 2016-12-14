package core.procedures.characters;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMBoolean;

import java.util.function.Predicate;

@FnArgs(args = {Character.class})
public class CharPredicate extends AFn {

  public static final CharPredicate IS_CHAR_WHITESPACE = new CharPredicate("char-whitespace?", Character::isWhitespace);
  public static final CharPredicate IS_CHAR_ALPHABETIC = new CharPredicate("char-alphabetic?", Character::isAlphabetic);
  public static final CharPredicate IS_CHAR_UPPER_CASE = new CharPredicate("char-upper-case?", Character::isUpperCase);
  public static final CharPredicate IS_CHAR_LOWER_CASE = new CharPredicate("char-lower-case?", Character::isLowerCase);
  public static final CharPredicate IS_CHAR_NUMERIC    = new CharPredicate("char-numeric?",    Character::isDigit);

  private final String name;
  private final Predicate<Character> predicate;

  private CharPredicate(String name, Predicate<Character> predicate) {
    this.name = name;
    this.predicate = predicate;
  }

  @Override
  public Object apply(Object... args) {
    return SCMBoolean.toSCMBoolean(predicate.test((Character)args[0]));
  }

  @Override
  public String getName() {
    return name;
  }
}
