package core.procedures.strings;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;

import java.util.List;

public class MakeString extends SCMProcedure {

  private static final SCMSymbol size  = new SCMSymbol("size");
  private static final SCMSymbol value = new SCMSymbol("value");
  private static final List<SCMSymbol> params = SCMCons.list(size, value);

  public MakeString() {
    super("make-string", params, null, null, true);
  }

  @Override
  public String apply(IEvaluator evaluator, IEnvironment env) {

    Object o = env.get(size);
    if (!(o instanceof Long)) {
      throw new WrongTypeException("Integer", o);
    }
    Long s = (Long)o;
    if (s < 0) {
      throw new IllegalArgumentException("Size value is out of range in `make-string`");
    }
    List vals = (List)env.get(value);
    if (vals.size() > 1) {
      throw new ArityException(vals.size() + 1, "make-string");
    }
    Object c = vals.isEmpty() ? Character.MIN_VALUE : vals.get(0);
    if (!(c instanceof Character)) {
      throw new WrongTypeException("Character", c);
    }
    StringBuilder sb = new StringBuilder();
    for (long i = 0; i < s; i++) {
      sb.append(c);
    }
    return sb.toString();
  }
}
