package core.procedures.cons;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.SCMBoolean;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;

import java.util.List;

public class Append extends SCMProcedure {

  private static final SCMSymbol args = new SCMSymbol("args");
  private static final List<SCMSymbol> params = SCMCons.list(args);

  public Append() {
    super("append", params, null, null, true);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    List es = (List)env.get(args);
    if (es.isEmpty()) {
      return SCMCons.NIL;
    }
    if (es.size() == 1) {
      return es.get(0);
    }
    Object r = es.get(0);
    if (!SCMCons.isList(r)) {
      throw new IllegalArgumentException(String.format("Wrong argument type. Expected: List, actual: %s", r));
    }
    Object result = (List)r;
    for (int i = 1; i < es.size(); i++) {
      Object cur = es.get(i);
      if ((i != es.size() - 1) && !SCMCons.isList(cur)) {
        throw new IllegalArgumentException(String.format("Wrong argument type. Expected: List, actual: %s", cur));
      }
      result = append2(result, cur);
    }
    return result;
  }

  // FIXME Make iterative
  public static Object append2(Object first, Object second) {

    if (SCMBoolean.valueOf(IsNull.isNull(first))) {
      return second;
    }
    return SCMCons.cons(Car.car(first), append2(Cdr.cdr(first), second));
  }
}
