package core.procedures.cons;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.IPair;
import core.scm.SCMList;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;

import java.util.List;

public class Car extends SCMProcedure {

  private static final SCMSymbol car = new SCMSymbol("car");
  private static final List<SCMSymbol> params = new SCMList<SCMSymbol>(car);

  public Car() {
    super("car", params, null, null, false);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    Object o = env.get(car);
    if (o instanceof IPair) {
      return ((IPair)o).car();
    }
    throw new IllegalArgumentException(String.format("Wrong argument type. Expected: Pair, actual: %s",
                                       o.getClass().getSimpleName()));
  }
}
