package main.core.procedures.math.bool;

import main.core.ast.SCMBoolean;
import main.core.exceptions.ArityException;
import main.core.procedures.AFn;

public class Negation extends AFn implements IBooleanOperation {

  @Override
  public Object invoke(Object... args) {
    if (args != null && args.length == 1) {
      return SCMBoolean.toSCMBoolean(!SCMBoolean.valueOf(args[0]));
    }
    throw new ArityException(args.length, "not");
  }

  public Boolean zero() {
    throw new ArityException(0, "not");
  }

  public Boolean apply(Boolean first, Boolean second) {
    throw new ArityException(2, "not");
  }

  public Object apply(Object first, Object second) {
    throw new ArityException(2, "not");
  }

  @Override
  public Object call() throws Exception {
    return invoke();
  }

  @Override
  public void run() {
    invoke();
  }
}
