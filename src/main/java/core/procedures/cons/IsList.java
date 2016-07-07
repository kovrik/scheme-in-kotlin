package core.procedures.cons;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.*;

import java.util.List;

public class IsList extends SCMProcedure {

  private static final SCMSymbol obj = new SCMSymbol("obj");
  private static final List<SCMSymbol> params = SCMCons.list(obj);

  public IsList() {
    super("list?", params, null, null, false);
  }

  @Override
  public SCMBoolean apply(IEvaluator evaluator, IEnvironment env) {
    Object o = env.get(obj);
    return isList(o);
  }

  public static SCMBoolean isList(Object object) {
    if (object instanceof ICons) {
      return SCMBoolean.toSCMBoolean(((ICons)object).isList());
    }
    return SCMBoolean.toSCMBoolean(object instanceof List);
  }
}
