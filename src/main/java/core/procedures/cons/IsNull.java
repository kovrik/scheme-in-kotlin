package core.procedures.cons;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.*;

import java.util.List;

public class IsNull extends SCMProcedure {

  private static final SCMSymbol obj = new SCMSymbol("obj");
  private static final List<SCMSymbol> params = new SCMList<SCMSymbol>(obj);

  public IsNull() {
    super("null?", params, null, null, false);
  }

  @Override
  public SCMBoolean apply(IEvaluator evaluator, IEnvironment env) {
    Object o = env.get(obj);
    return isNull(o);
  }

  public static SCMBoolean isNull(Object object) {
    if (object instanceof IPair) {
      return SCMBoolean.toSCMBoolean(SCMList.NIL.equals(object));
    }
    return SCMBoolean.FALSE;
  }
}
