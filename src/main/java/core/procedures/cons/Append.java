package core.procedures.append;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;

import java.util.List;

public class Append extends SCMProcedure {

  private static final SCMSymbol first = new SCMSymbol("first");
  private static final SCMSymbol second = new SCMSymbol("second");
  private static final List<SCMSymbol> params = SCMCons.list(first, second);

  public Append() {
    super("append", params, null, null, false);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    // TODO
//    Object first = env.get(Append.first);
//    Object second = env.get(Append.second);
//    if (second instanceof SCMList) {
//      // do not modify the original list, but return new one
//      SCMList list = new SCMList((SCMList)second);
//      list.push(first);
//      return list;
//    }
//    return SCMCons.append(first, second);
    return null;
  }
}
