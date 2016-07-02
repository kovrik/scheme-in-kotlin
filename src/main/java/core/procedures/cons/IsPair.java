package core.procedures.cons;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.*;

import java.util.List;

public class IsPair extends SCMProcedure {

  private static final SCMSymbol o = new SCMSymbol("o");
  private static final List<SCMSymbol> params = new SCMList<SCMSymbol>(o);

  public IsPair() {
    super("pair?", params, null, null, false);
  }

  @Override
  public SCMBoolean apply(IEvaluator evaluator, IEnvironment env) {
    Object o = env.get(this.o);
    if (o instanceof IPair) {
      return SCMBoolean.toSCMBoolean(((IPair)o).isPair());
    }
    return SCMBoolean.FALSE;
  }
}
