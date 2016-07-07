package core.procedures.cons;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.*;

import java.util.List;

public class Car extends SCMProcedure {

  private static final SCMSymbol car = new SCMSymbol("car");
  private static final List<SCMSymbol> params = SCMCons.list(car);

  public Car() {
    super("car", params, null, null, false);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    Object o = env.get(car);
    return car(o);
  }

  public static Object car(Object o) {
    if (o instanceof ICons) {
      return ((ICons)o).car();
    }
    if (o instanceof List) {
      List list = (List) o;
      if (list.isEmpty()) {
        throw new IllegalArgumentException("Wrong argument type. Expected: Pair, actual: ()");
      }
      return list.get(0);
    }
    throw new IllegalArgumentException(String.format("Wrong argument type. Expected: Pair, actual: %s",
        o.getClass().getSimpleName()));
  }
}
