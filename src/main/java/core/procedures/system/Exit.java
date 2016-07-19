package core.procedures.system;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.WrongTypeException;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;
import core.scm.specialforms.SCMSpecialForm;

import java.util.List;

public class Exit extends SCMProcedure {

  private static final SCMSymbol exitCode = new SCMSymbol("code");
  private static final List<SCMSymbol> params = SCMCons.list(exitCode);

  public Exit() {
    super("exit", params, null, null, true);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    List l = (List)env.get(exitCode);
    if (l.isEmpty()) {
      System.exit(0);
    } else {
      Object o = l.get(0);
      if (!(l.get(0) instanceof Long)) {
        throw new WrongTypeException("Integer", o);
      }
      System.exit(((Long)o).intValue());
    }
    return SCMSpecialForm.UNSPECIFIED;
  }
}
