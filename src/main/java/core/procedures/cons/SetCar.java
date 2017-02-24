package core.procedures.cons;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMClass;

import java.util.List;

import static core.scm.SCMConstant.UNSPECIFIED;

public final class SetCar extends AFn {

  public SetCar() {
    super(new FnArgsBuilder().minArgs(2).maxArgs(2)
                             .mandatoryArgsTypes(new Class[]{SCMClass.SCMPair.class, Object.class}));
  }

  @Override
  public String getName() {
    return "set-car!";
  }

  @Override
  public Object apply2(Object arg1, Object arg2) {
    ((List)arg1).set(0, arg2);
    return UNSPECIFIED;
  }
}
