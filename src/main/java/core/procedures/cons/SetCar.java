package core.procedures.cons;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMClass;

import java.util.List;

import static core.scm.SCMUnspecified.UNSPECIFIED;

@FnArgs(args = {SCMClass.SCMPair.class, Object.class})
public class SetCar extends AFn {

  @Override
  public String getName() {
    return "set-car!";
  }

  @Override
  public Object apply(Object... args) {
    ((List)args[0]).set(0, args[1]);
    return UNSPECIFIED;
  }
}
