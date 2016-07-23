package core.procedures.strings;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.WrongTypeException;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;

import java.util.List;

public class StringAppend extends SCMProcedure {

  private static final SCMSymbol strs = new SCMSymbol("strs");
  private static final List<SCMSymbol> params = SCMCons.list(strs);

  public StringAppend() {
    super("string-append", params, null, null, true);
  }

  @Override
  public String apply(IEvaluator evaluator, IEnvironment env) {
    List strs = (List)env.get(StringAppend.strs);
    if (strs.isEmpty()) {
      return "";
    }
    if (strs.size() == 1) {
      Object o = strs.get(0);
      if (!(o instanceof String)) {
        throw new WrongTypeException("String", o);
      }
      return (String)o;
    }
    StringBuilder sb = new StringBuilder();
    for (Object str : strs) {
      if (!(str instanceof String)) {
        throw new WrongTypeException("String", str);
      }
      sb.append(str);
    }
    return sb.toString();
  }
}
