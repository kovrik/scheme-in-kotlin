package core.procedures.characters;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.function.Predicate;

public final class CharPredicate extends AFn {

  public static final CharPredicate IS_CHAR_WHITESPACE = new CharPredicate("char-whitespace?", Character::isWhitespace);
  public static final CharPredicate IS_CHAR_ALPHABETIC = new CharPredicate("char-alphabetic?", Character::isAlphabetic);
  public static final CharPredicate IS_CHAR_UPPER_CASE = new CharPredicate("char-upper-case?", Character::isUpperCase);
  public static final CharPredicate IS_CHAR_LOWER_CASE = new CharPredicate("char-lower-case?", Character::isLowerCase);
  public static final CharPredicate IS_CHAR_NUMERIC    = new CharPredicate("char-numeric?",    Character::isDigit);

  private final String name;
  private final Predicate<Character> predicate;

  private CharPredicate(String name, Predicate<Character> predicate) {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{Character.class}));
    this.name = name;
    this.predicate = predicate;
  }

  @Override
  public Boolean apply1(Object arg) {
    return predicate.test((Character)arg);
  }

  @Override
  public String getName() {
    return name;
  }
}
