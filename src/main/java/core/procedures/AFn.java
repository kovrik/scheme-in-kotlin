package core.procedures;

import core.exceptions.ArityException;

public abstract class AFn implements IFn<Object[], Object> {

  /* Return true if function is pure (referentially transparent),
   * false otherwise. */
  public boolean isPure() {
    return false;
  }

  @Override
  public Object apply(Object... args) {
    throw new ArityException(args.length, getName());
  }

  public String getName() {
    return getClass().getSimpleName();
  }

  @Override
  public String toString() {
    String name = getName();
    if (name == null || name.isEmpty()) {
      return "#<procedure>";
    }
    return "#<procedure:" + name + ">";
  }
}
