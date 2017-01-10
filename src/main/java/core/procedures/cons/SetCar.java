package core.procedures.cons;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMClass;

import java.util.List;

import static core.scm.SCMUnspecified.UNSPECIFIED;

@FnArgs(minArgs = 2, maxArgs = 2, mandatoryArgsTypes = {SCMClass.SCMPair.class, Object.class})
public class SetCar extends AFn {

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
