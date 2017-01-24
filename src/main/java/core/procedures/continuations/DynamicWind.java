package core.procedures.continuations;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.procedures.AFn;
import core.procedures.IFn;
import core.scm.FnArgs;
import core.scm.SCMCons;

@FnArgs(minArgs = 3, maxArgs = 3, mandatoryArgsTypes = {IFn.class, IFn.class, IFn.class})
public final class DynamicWind extends AFn {

  @Override
  public boolean isPure() {
    return false;
  }

  @Override
  public String getName() {
    return "dynamic-wind";
  }

  @Override
  public Object apply3(Object arg1, Object arg2, Object arg3) {
    throw new UnsupportedOperationException(getName() + ": must be evaluated in Evaluator!");
  }

  /* Actual dynamic-wind */
  public Object dynamicWind(IFn pre, IFn value, IFn post, Environment env, Evaluator evaluator) {
    /* Evaluate before-thunk first */
    evaluator.eval(SCMCons.list(pre), env);
    try {
      /* Evaluate and return value-thunk */
      return evaluator.eval(SCMCons.list(value), env);
    } finally {
      /* Finally, evaluate post-thunk */
      evaluator.eval(SCMCons.list(post), env);
    }
  }
}
