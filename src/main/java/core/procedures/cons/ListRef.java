package core.procedures.cons;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;
import core.writer.Writer;

import java.util.List;

public class ListRef extends SCMProcedure {

  private static final SCMSymbol lst = new SCMSymbol("list");
  private static final SCMSymbol pos = new SCMSymbol("pos");
  private static final List<SCMSymbol> params = SCMCons.list(lst, pos);

  public ListRef() {
    super("list-ref", params, null, null, false);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    Object p = env.get(pos);
    if (!(p instanceof Long)) {
      throw new IllegalArgumentException(String.format("Wrong argument type. Expected: Integer, actual: %s", Writer.write(p)));
    }
    Object o = env.get(lst);
    Long p1 = (Long) p;
    if (!(o instanceof List)) {
      throw new IllegalArgumentException(String.format("Wrong argument type. Expected: List, actual: %s", Writer.write(o)));
    }
    List list = (List)o;
    if (p1 >= list.size()) {
      throw new IllegalArgumentException("Value out of range: " + p1);
    }
    /* Cons cell */
    if ((list instanceof SCMCons) && !((SCMCons)list).isList()) {
      if (p1 == 0) {
        return ((SCMCons)list).car();
      } else {
        throw new IllegalArgumentException(String.format("Wrong argument type. Expected: List, actual: %s", Writer.write(o)));
      }
    }
    return list.get(p1.intValue());
  }
}
