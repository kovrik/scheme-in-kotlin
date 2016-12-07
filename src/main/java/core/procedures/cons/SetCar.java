package core.procedures.cons;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMCons;

import java.util.List;

import static core.scm.SCMUnspecified.UNSPECIFIED;

@FnArgs(args = {SCMCons.SCMPair.class, Object.class})
public class SetCar extends AFn {

  @Override
  public String getName() {
    return "set-car!";
  }

  @Override
  public Object invoke(Object... args) {
    ((List)args[0]).set(0, args[1]);
    return UNSPECIFIED;
  }
}
