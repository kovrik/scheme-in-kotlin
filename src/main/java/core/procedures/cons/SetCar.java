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
  private static final SCMSymbol object = new SCMSymbol("object");
  private static final List<SCMSymbol> params = SCMCons.list(pair, object);

  public SetCar() {
    super("set-car!", params, null, null, false);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {

    Object p = env.get(pair);
    if ((!(p instanceof SCMCons)) || !((SCMCons)p).isPair()) {
      throw new IllegalArgumentException(String.format("Wrong argument type. Expected: Pair, actual: %s",
                                                       p));
    }
    SCMCons cons = (SCMCons)p;
    Object o = env.get(object);
    cons.set(0, o);
    return SCMSpecialForm.UNSPECIFIED;
  }
}
