package core.procedures.strings;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.WrongTypeException;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;

import java.util.List;

public class ListToString extends SCMProcedure {

  private static final SCMSymbol lst = new SCMSymbol("lst");
  private static final List<SCMSymbol> params = SCMCons.list(lst);

  public ListToString() {
    super("list->string", params, null, null, false);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    Object o = env.get(lst);
    if (!(o instanceof List) || ((o instanceof SCMCons) && !((SCMCons)o).isList())) {
      throw new WrongTypeException("List", o);
    }

    List cs = (List)env.get(lst);
    if (cs.isEmpty()) {
      return "";
    }
    StringBuilder sb = new StringBuilder(cs.size());
    for (Object c : cs) {
      if (!(c instanceof Character)) {
        throw new WrongTypeException("Character", c);
      }
      sb.append(c);
    }
    return sb.toString();
  }
}
