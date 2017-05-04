package core.procedures.cons;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMClass;
import core.scm.SCMVoid;

import java.util.List;

public final class SetCar extends AFn {

  public SetCar() {
    super(new FnArgsBuilder().min(2).max(2)
                             .mandatory(new Class[]{SCMClass.SCMPair.class, Object.class}).build());
  }

  @Override
  public String getName() {
    return "set-car!";
  }

  @Override
  public Object apply2(Object arg1, Object arg2) {
    ((List)arg1).set(0, arg2);
    return SCMVoid.VOID;
  }
}
