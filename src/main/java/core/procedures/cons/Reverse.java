package core.procedures.cons;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;
import core.writer.Writer;

import java.util.List;

public class Reverse extends SCMProcedure {

  private static final SCMSymbol lst = new SCMSymbol("list");
  private static final List<SCMSymbol> params = SCMCons.list(lst);

  public Reverse() {
    super("reverse", params, null, null, false);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    Object l = env.get(lst);
    if (!SCMCons.isList(l)) {
      throw new IllegalArgumentException(String.format("Wrong argument type. Expected: List, actual: %s", Writer.write(l)));
    }
    List list = (List)l;
    SCMCons<Object> result = SCMCons.list();
    for (Object o : list) {
      result.push(o);
    }
    return result;
  }
}
