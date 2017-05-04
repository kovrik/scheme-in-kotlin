package core.procedures.delayed;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.IDeref;

public final class Deref extends AFn {

  public Deref() {
    super(new FnArgsBuilder().min(1).max(1).build());
  }

  @Override
  public String getName() {
    return "deref";
  }

  @Override
  public Object apply1(Object arg) {
    if (!(arg instanceof IDeref)) {
      throw new WrongTypeException(getName(), "Delay or Promise or Future", arg);
    }
    return ((IDeref) arg).deref();
  }
}
