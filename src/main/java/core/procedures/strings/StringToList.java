package core.procedures.strings;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;

import java.util.List;

public class StringToList extends SCMProcedure {

  private static final SCMSymbol str = new SCMSymbol("str");
  private static final List<SCMSymbol> params = SCMCons.list(str);

  public StringToList() {
    super("string->list", params, null, null, false);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    Object o = env.get(str);
    if (!(o instanceof String)) {
      throw new IllegalArgumentException("Wrong argument type. Expected: String, actual: " + o);
    }
    SCMCons<Character> list = SCMCons.list();
    for (char c : ((String)o).toCharArray()) {
      list.add(c);
    }
    return list;
  }
}