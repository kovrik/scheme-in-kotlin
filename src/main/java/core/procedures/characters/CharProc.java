package core.procedures.characters;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.function.Function;

public final class CharProc extends AFn {

  public static final CharProc CHAR_TO_INTEGER = new CharProc("char->integer", ch -> (long)ch);
  public static final CharProc CHAR_UPCASE     = new CharProc("char-upcase", Character::toUpperCase);
  public static final CharProc CHAR_DOWNCASE   = new CharProc("char-downcase", Character::toLowerCase);

  private final String name;
  private final Function<Character, Object> function;

  private CharProc(String name, Function<Character, Object> function) {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{Character.class}));
    this.name = name;
    this.function = function;
  }

  @Override
  public Object apply1(Object arg) {
    return function.apply((Character) arg);
  }

  @Override
  public String getName() {
    return name;
  }
}
