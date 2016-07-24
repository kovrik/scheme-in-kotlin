package core.procedures.strings;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;

import java.util.List;

public class StringToNumber extends SCMProcedure {

  private static final SCMSymbol str = new SCMSymbol("str");
  private static final SCMSymbol rad = new SCMSymbol("rad");
  private static final List<SCMSymbol> params = SCMCons.list(str, rad);

  public StringToNumber() {
    super("string->number", params, null, null, true);
  }

  @Override
  public String apply(IEvaluator evaluator, IEnvironment env) {
    Object o = env.get(str);
    if (!(o instanceof String)) {
      throw new WrongTypeException("String", o);
    }

    /* Read number */
    Integer radix  = null;
    // TODO

    /* Get radix */
    List r = (List)env.get(rad);
    if (r.size() > 1) {
      throw new ArityException(r.size() + 1, "string->number");
    }
    Object o1 = null;
    if (r.size() == 1) {
      o1 = r.get(0);
      if (!(o1 instanceof Long)) {
        throw new WrongTypeException("Integer", o);
      }
      if (!(o1.equals(2L) || o1.equals(8L) || o1.equals(10L) || o1.equals(16L))) {
        throw new IllegalArgumentException("Wrong radix: " + o1);
      }
    }

    // TODO
    /* Convert */
    if (radix == null) {
      if (o1 != null) {
        radix = (Integer)o1;
      } else {
        radix = 10;
      }
    }

    return o.toString();
  }
}