package core.procedures.cons;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.SCMCons;
import core.scm.SCMList;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;

import java.util.List;

public class Cons extends SCMProcedure {

  private static final SCMSymbol car = new SCMSymbol("car");
  private static final SCMSymbol cdr = new SCMSymbol("cdr");
  private static final List<SCMSymbol> params = new SCMList<SCMSymbol>(car, cdr);

  public Cons() {
    super("cons", params, null, null, false);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    Object car = env.get(Cons.car);
    Object cdr = env.get(Cons.cdr);
    if (cdr instanceof SCMList) {
      // do not modify the original list, but return new one
      SCMList list = new SCMList((SCMList)cdr);
      list.push(car);
      return list;
    }
    return SCMCons.cons(car, cdr);
  }
}
