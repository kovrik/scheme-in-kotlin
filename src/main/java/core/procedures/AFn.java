package core.procedures;

import core.exceptions.ArityException;
import core.scm.SCMClass;

public abstract class AFn implements IFn {

  /* Return true if function is pure (referentially transparent),
   * false otherwise. */
  public boolean isPure() {
    return false;
  }

  @Override
  public Object invoke(Object... args) {
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

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.PROCEDURE;
  }
}
