package core.procedures;

import core.exceptions.ArityException;
import core.scm.ISCMClass;
import core.scm.SCMClass;

public abstract class AFn implements IFn, ISCMClass {

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

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.PROCEDURE;
  }
}
