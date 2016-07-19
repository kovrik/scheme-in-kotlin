package core.procedures.strings;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.WrongTypeException;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;

import java.util.List;

public class StringSet extends SCMProcedure {

  private static final SCMSymbol string  = new SCMSymbol("string");
  private static final SCMSymbol pos = new SCMSymbol("pos");
  private static final SCMSymbol c = new SCMSymbol("c");
  private static final List<SCMSymbol> params = SCMCons.list(string, pos, c);

  public StringSet() {
    super("string-set!", params, null, null, false);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {

    Object o = env.get(string);
    if (!(o instanceof String)) {
      throw new WrongTypeException("String", o);
    }
    String str = (String)o;

    Object p = env.get(pos);
    if (!(p instanceof Long)) {
      throw new WrongTypeException("Integer", p);
    }
    Long pos = (Long)p;
    if ((pos < 0) || (pos >= str.length())) {
      throw new IllegalArgumentException(String.format("Value out of range: %s", pos));
    }
    Object ch = env.get(c);
    if (!(ch instanceof Character)) {
      throw new WrongTypeException("Character", ch);
    }
    Character character = (Character) ch;
    String before = str.substring(0, pos.intValue());
    String after = str.substring(pos.intValue() + 1, str.length());
    return before + character + after;
    // FIXME Make modifiable string!
//    return SCMSpecialForm.UNSPECIFIED;
  }
}
