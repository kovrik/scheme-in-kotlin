package core.procedures.cons;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.*;

import java.util.List;

public class ConsProc extends SCMProcedure {

  private static final SCMSymbol car = new SCMSymbol("car");
  private static final SCMSymbol cdr = new SCMSymbol("cdr");
  private static final List<SCMSymbol> params = new SCMList<SCMSymbol>(car, cdr);

  public ConsProc() {
    super("cons", params, null, null, false);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    Object car = env.get(ConsProc.car);
    Object cdr = env.get(ConsProc.cdr);
    return cons(car, cdr);
  }

  public static Object cons(Object car, Object cdr) {
    if (car == null && cdr == null) {
      return SCMList.NIL;
    }
    if (cdr instanceof IPair) {
      IPair pair = (IPair) cdr;
      return pair.cons(car);
    }
    return new SCMCons(car, cdr);
  }
}
