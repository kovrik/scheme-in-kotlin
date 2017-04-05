package core.procedures.reflection;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

public final class CharType extends AFn {

  public CharType() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "char";
  }

  @Override
  public Character apply1(Object arg) {
    // FIXME?
    /* Have to box it */
    if (arg instanceof Number) {
      return (char)((Number) arg).intValue();
    } else if (arg instanceof Character) {
      return (Character) arg;
    }
    throw new WrongTypeException("char", "Character or Number", arg);
  }
}
