package core.procedures.cons;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;
import core.scm.specialforms.SCMSpecialForm;

import java.util.List;

public class SetCar extends SCMProcedure {

  private static final SCMSymbol pair  = new SCMSymbol("pair");
  private static final SCMSymbol car = new SCMSymbol("car");
  private static final List<SCMSymbol> params = SCMCons.list(pair, car);

  public SetCar() {
    super("set-car!", params, null, null, false);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    Object p = env.get(pair);
    if (!SCMCons.isPair(p)) {
      throw new IllegalArgumentException(String.format("Wrong argument type. Expected: Pair, actual: %s", p));
    }
    List cons = (List)p;
    cons.set(0, env.get(car));
    return SCMSpecialForm.UNSPECIFIED;
  }
}
