package core.procedures.strings;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;

import java.util.List;

public class StringRef extends SCMProcedure {

  private static final SCMSymbol string  = new SCMSymbol("string");
  private static final SCMSymbol pos = new SCMSymbol("pos");
  private static final List<SCMSymbol> params = SCMCons.list(string, pos);

  public StringRef() {
    super("string-ref", params, null, null, false);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {

    Object o = env.get(string);
    if (!(o instanceof String)) {
      throw new IllegalArgumentException(String.format("Wrong argument type. Expected: String, actual: %s", o));
    }
    String s = (String)o;

    Object p = env.get(pos);
    if (!(p instanceof Long)) {
      throw new IllegalArgumentException(String.format("Wrong argument type. Expected: Integer, actual: %s", p));
    }
    Long pos = (Long)p;
    if ((pos < 0) || (pos >= s.length())) {
      throw new IllegalArgumentException(String.format("Value out of range: %s", pos));
    }
    return s.charAt(pos.intValue());
  }
}
