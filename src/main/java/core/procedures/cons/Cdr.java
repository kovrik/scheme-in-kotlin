package core.procedures.cons;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.IPair;
import core.scm.SCMList;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;

import java.util.List;

public class Cdr extends SCMProcedure {

  private static final SCMSymbol cdr = new SCMSymbol("cdr");
  private static final List<SCMSymbol> params = new SCMList<SCMSymbol>(cdr);

  public Cdr() {
    super("cdr", params, null, null, false);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    Object o = env.get(cdr);
    if (o instanceof IPair) {
      return ((IPair)o).cdr();
    }
    throw new IllegalArgumentException(String.format("Wrong argument type. Expected: Pair, actual: %s",
                                       o.getClass().getSimpleName()));
  }
}
