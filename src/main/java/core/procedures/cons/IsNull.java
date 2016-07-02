package core.procedures.cons;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.*;

import java.util.List;

public class IsNull extends SCMProcedure {

  private static final SCMSymbol o = new SCMSymbol("o");
  private static final List<SCMSymbol> params = new SCMList<SCMSymbol>(o);

  public IsNull() {
    super("null?", params, null, null, false);
  }

  @Override
  public SCMBoolean apply(IEvaluator evaluator, IEnvironment env) {
    Object o = env.get(this.o);
    if (o instanceof IPair) {
      return SCMBoolean.toSCMBoolean(((IPair)o).isNull());
    }
    return SCMBoolean.FALSE;
  }
}
