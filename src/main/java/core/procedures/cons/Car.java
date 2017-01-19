package core.procedures.cons;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.ICons;
import core.scm.SCMClass;

import java.util.List;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {SCMClass.SCMPair.class})
public final class Car extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "car";
  }

  @Override
  public Object apply1(Object arg) {
    return car(arg);
  }

  public static Object car(Object o) {
    if (o instanceof ICons) {
      return ((ICons)o).car();
    }
    return ((List)o).get(0);
  }
}
