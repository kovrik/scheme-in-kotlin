package core.procedures.vectors;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.SCMList;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;
import core.scm.SCMVector;

import java.util.List;

public class Vector extends SCMProcedure {

  private static final SCMSymbol values = new SCMSymbol("values");
  private static final List<SCMSymbol> params = new SCMList<SCMSymbol>(values);

  public Vector() {
    super("vector", params, null, null, true);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    SCMList vals = (SCMList)env.get(values);
    if (vals.isEmpty()) {
      return new SCMVector();
    }
    return new SCMVector(vals.toArray());
  }
}
