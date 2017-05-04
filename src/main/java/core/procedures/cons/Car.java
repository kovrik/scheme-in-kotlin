package core.procedures.cons;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.ICons;
import core.scm.SCMClass;

import java.util.List;

public class Car extends AFn {

  public Car() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{SCMClass.SCMPair.class}).build());
  }

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
