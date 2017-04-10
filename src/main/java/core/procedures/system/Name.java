package core.procedures.system;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.INamed;

public final class Name extends AFn {

  public Name() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "name";
  }

  @Override
  public CharSequence apply1(Object arg) {
    if (arg instanceof INamed) {
      return ((INamed)arg).getName();
    }
    if (arg instanceof CharSequence) {
      return (CharSequence) arg;
    }
    throw new WrongTypeException(getName(), "String or Symbol or Keyword", arg);
  }
}
