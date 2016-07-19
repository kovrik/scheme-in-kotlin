package core.procedures.cons;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.WrongTypeException;
import core.scm.ICons;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;

import java.util.List;

public class Cdr extends SCMProcedure {

  private static final SCMSymbol cdr = new SCMSymbol("cdr");
  private static final List<SCMSymbol> params = SCMCons.list(cdr);

  public Cdr() {
    super("cdr", params, null, null, false);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    Object o = env.get(cdr);
    return cdr(o);
  }

  public static Object cdr(Object o) {
    if (o instanceof ICons) {
      return ((ICons)o).cdr();
    }
    if (o instanceof List) {
      List list = (List) o;
      if (list.isEmpty()) {
        throw new WrongTypeException("Pair", list);
      }
      return list.subList(1, list.size());
    }
    throw new WrongTypeException("Pair", o);
  }
}
