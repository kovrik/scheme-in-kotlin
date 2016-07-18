package core.procedures.cons;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;
import core.writer.Writer;

import java.util.List;

public class ListTail extends SCMProcedure {

  private static final SCMSymbol lst = new SCMSymbol("list");
  private static final SCMSymbol pos = new SCMSymbol("pos");
  private static final List<SCMSymbol> params = SCMCons.list(lst, pos);

  public ListTail() {
    super("list-tail", params, null, null, false);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    Object p = env.get(pos);
    if (!(p instanceof Long)) {
      throw new IllegalArgumentException(String.format("Wrong argument type. Expected: Integer, actual: %s", Writer.write(p)));
    }
    Object o = env.get(lst);
    Long p1 = (Long) p;
    if (p1 == 0) {
      return o;
    }
    if (!(o instanceof List)) {
      throw new IllegalArgumentException(String.format("Wrong argument type. Expected: List, actual: %s", Writer.write(o)));
    }
    List list = (List)o;
    if (p1 >= list.size() + 1) {
      throw new IllegalArgumentException("Value out of range: " + p1);
    }
    /* Cons cell */
    if ((list instanceof SCMCons) && !((SCMCons)list).isList()) {
      if (p1 == 1) {
        return ((SCMCons)list).cdr();
      } else {
        throw new IllegalArgumentException("Value out of range: " + p1);
      }
    }
    return list.subList(p1.intValue(), list.size());
  }
}
