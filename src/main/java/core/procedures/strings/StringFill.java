package core.procedures.strings;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.WrongTypeException;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;

import java.util.List;

public class StringFill extends SCMProcedure {

  private static final SCMSymbol string  = new SCMSymbol("string");
  private static final SCMSymbol ch = new SCMSymbol("ch");
  private static final List<SCMSymbol> params = SCMCons.list(string, ch);

  public StringFill() {
    super("string-fill!", params, null, null, false);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    Object o = env.get(string);
    if (!(o instanceof String)) {
      throw new WrongTypeException("String", o);
    }
    String s = (String)o;

    Object c = env.get(ch);
    if (!(c instanceof Character)) {
      throw new WrongTypeException("Character", c);
    }
    StringBuilder sb = new StringBuilder(s.length());
    for (int i = 0; i < s.length(); i++) {
      sb.append(c);
    }
    return sb.toString();
  }
}
