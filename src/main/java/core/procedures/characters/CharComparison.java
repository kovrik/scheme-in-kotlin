package core.procedures.characters;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.function.BiPredicate;

public final class CharComparison extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  public static final CharComparison CHAR_EQ = new CharComparison("char=?", (arg1, arg2) -> arg1.compareTo(arg2) == 0);
  public static final CharComparison CHAR_LE = new CharComparison("char<?", (arg1, arg2) -> arg1.compareTo(arg2) < 0);
  public static final CharComparison CHAR_GR = new CharComparison("char>?", (arg1, arg2) -> arg1.compareTo(arg2) > 0);
  public static final CharComparison CHAR_LE_OR_EQ = new CharComparison("char<=?", (arg1, arg2) -> arg1.compareTo(arg2) <= 0);
  public static final CharComparison CHAR_GR_OR_EQ = new CharComparison("char>=?", (arg1, arg2) -> arg1.compareTo(arg2) >= 0);
  public static final CharComparison CHAR_EQ_CI = new CharComparison("char-ci=?", (arg1, arg2) -> Character.toLowerCase(arg1) == Character.toLowerCase(arg2));
  public static final CharComparison CHAR_LE_CI = new CharComparison("char-ci<?", (arg1, arg2) -> Character.toLowerCase(arg1) <  Character.toLowerCase(arg2));
  public static final CharComparison CHAR_GR_CI = new CharComparison("char-ci>?", (arg1, arg2) -> Character.toLowerCase(arg1) >  Character.toLowerCase(arg2));
  public static final CharComparison CHAR_LE_OR_EQ_CI = new CharComparison("char-ci<=?", (arg1, arg2) -> Character.toLowerCase(arg1) <= Character.toLowerCase(arg2));
  public static final CharComparison CHAR_GR_OR_EQ_CI = new CharComparison("char-ci>=?", (arg1, arg2) -> Character.toLowerCase(arg1) >= Character.toLowerCase(arg2));

  private final String name;
  private final BiPredicate<Character, Character> predicate;

  private CharComparison(String name, BiPredicate<Character, Character> predicate) {
    super(new FnArgsBuilder().minArgs(2)
                             .mandatoryArgsTypes(new Class[]{Character.class, Character.class})
                             .restArgsType(Character.class));
    this.name = name;
    this.predicate = predicate;
  }

  @Override
  public Boolean apply(Object... args) {
    for (int i = 0; i < args.length - 1; i++) {
      if ((!predicate.test((Character) args[i], (Character) args[i + 1]))) {
        return Boolean.FALSE;
      }
    }
    return Boolean.TRUE;
  }

  @Override
  public String getName() {
    return name;
  }
}
