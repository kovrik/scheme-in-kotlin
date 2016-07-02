package core.procedures.cons;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.ArityException;
import core.scm.SCMCons;
import core.scm.SCMList;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;

import java.util.List;

public class Cons extends SCMProcedure {

  private static final SCMSymbol values = new SCMSymbol("values");
  private static final List<SCMSymbol> params = new SCMList<SCMSymbol>(values);

  public Cons() {
    super("cons", params, null, null, true);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    SCMList vals = (SCMList)env.get(values);
    if (vals.size() != 2) {
      throw new ArityException(vals.size(), 2, "cons");
    }
    Object first = vals.get(0);
    Object second = vals.get(1);
    if (second instanceof SCMList) {
      // do not modify the original list, but return new one
      SCMList list = new SCMList((SCMList)second);
      list.push(first);
      return list;
    }
    return SCMCons.cons(first, second);
  }
}
