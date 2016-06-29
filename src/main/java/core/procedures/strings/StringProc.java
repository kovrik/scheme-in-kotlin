package core.procedures.strings;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.SCMList;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;

import java.util.List;

public class StringProc extends SCMProcedure {

  private static final SCMSymbol chars = new SCMSymbol("chars");
  private static final List<SCMSymbol> params = new SCMList<SCMSymbol>(chars);

  public StringProc() {
    super("string", params, null, null, true);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    SCMList cs = (SCMList)env.get(chars);
    if (cs.isEmpty()) {
      return "";
    }
    StringBuilder sb = new StringBuilder(cs.size());
    for (Object c : cs) {
      if (!(c instanceof Character)) {
        throw new IllegalArgumentException("Wrong argument type. Expected: Character, actual: " + c.getClass().getSimpleName());
      }
      sb.append(c);
    }
    return sb.toString();
  }
}
