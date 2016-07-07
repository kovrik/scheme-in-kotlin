package core.procedures.cons;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.*;

import java.util.List;

public class IsPair extends SCMProcedure {

  private static final SCMSymbol obj = new SCMSymbol("obj");
  private static final List<SCMSymbol> params = SCMCons.list(obj);

  public IsPair() {
    super("pair?", params, null, null, false);
  }

  @Override
  public SCMBoolean apply(IEvaluator evaluator, IEnvironment env) {
    Object o = env.get(obj);
    return isPair(o);
  }

  public static SCMBoolean isPair(Object object) {
    if (object instanceof ICons) {
      return SCMBoolean.toSCMBoolean(((ICons)object).isPair());
    }
    if (object instanceof List) {
      return SCMBoolean.toSCMBoolean(!((List)object).isEmpty());
    }
    return SCMBoolean.FALSE;
  }
}
