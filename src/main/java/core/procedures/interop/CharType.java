package core.procedures.interop;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

public final class CharType extends AFn {

  public CharType() {
    super(new FnArgsBuilder().min(1).max(1).build());
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
    /* Have to box it */
    if (arg instanceof Number) {
      return (char)((Number) arg).intValue();
    } else if (arg instanceof Character) {
      return (Character) arg;
    }
    throw new WrongTypeException("char", "Character or Number", arg);
  }
}
