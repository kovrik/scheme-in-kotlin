package core.procedures;

import core.exceptions.ArityException;

public abstract class AFn implements IFn {

  @Override
  public Object invoke(Object... args) {
    return throwArity(args.length);
  }

  public Object throwArity(int n) {
    throw new ArityException(n, getName());
  }

  public String getName() {
    return getClass().getSimpleName();
  }

  @Override
  public String toString() {
    return "#<procedure " + getName() + ">";
  }
}
