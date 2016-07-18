package core.procedures.cons;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;
import core.scm.specialforms.SCMSpecialForm;
import core.writer.Writer;

import java.util.List;

public class SetCdr extends SCMProcedure {

  private static final SCMSymbol pair  = new SCMSymbol("pair");
  private static final SCMSymbol cdr = new SCMSymbol("object");
  private static final List<SCMSymbol> params = SCMCons.list(pair, cdr);

  public SetCdr() {
    super("set-cdr!", params, null, null, false);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    Object p = env.get(pair);
    if (!SCMCons.isPair(p)) {
      throw new IllegalArgumentException(String.format("Wrong argument type. Expected: Pair, actual: %s", Writer.write(p)));
    }

    List list = (List)p;
    /* Remove tail */
    list.subList(1, list.size()).clear();

    /* Set new tail */
    Object o = env.get(cdr);
    if (o instanceof List) {
      list.addAll((List)o);
    } else {
      list.add(o);
      // FIXME How to make it Cons if it is a SubList?
      if (list instanceof SCMCons) {
        ((SCMCons)list).setList(false);
      }
    }
    return SCMSpecialForm.UNSPECIFIED;
  }
}