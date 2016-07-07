package core.procedures.vectors;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.*;

import java.util.List;

public class Vector extends SCMProcedure {

  private static final SCMSymbol values = new SCMSymbol("values");
  private static final List<SCMSymbol> params = SCMCons.list(values);

  public Vector() {
    super("vector", params, null, null, true);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    List vals = (List)env.get(values);
    if (vals.isEmpty()) {
      return new SCMVector();
    }
    return new SCMVector(vals.toArray());
  }
}
