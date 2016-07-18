package core.procedures.strings;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;
import core.writer.Writer;

import java.util.List;

public class StringProc extends SCMProcedure {

  private static final SCMSymbol chars = new SCMSymbol("chars");
  private static final List<SCMSymbol> params = SCMCons.list(chars);

  public StringProc() {
    super("string", params, null, null, true);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    List cs = (List)env.get(chars);
    if (cs.isEmpty()) {
      return "";
      // TODO?
      /* Return new String object */
      // return new String();
    }
    StringBuilder sb = new StringBuilder(cs.size());
    for (Object c : cs) {
      if (!(c instanceof Character)) {
        throw new IllegalArgumentException("Wrong argument type. Expected: Character, actual: " + Writer.write(c));
      }
      sb.append(c);
    }
    return sb.toString();
  }
}
