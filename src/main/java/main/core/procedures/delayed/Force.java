package main.core.procedures.delayed;

import main.core.ast.SCMList;
import main.core.ast.SCMSymbol;
import main.core.environment.IEnvironment;
import main.core.evaluator.IEvaluator;
import main.core.procedures.Procedure;

public class Force extends Procedure {

  private static final SCMSymbol PROMISE = new SCMSymbol("promise");
  private static final SCMList<Object> FORCE = new SCMList<Object>(PROMISE);

  public Force() {
    /* (lambda (p) (p)) */
    super(FORCE, FORCE);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    Object promise = env.get(PROMISE);
    if (!(promise instanceof Promise)) {
      throw new IllegalArgumentException("Wrong type argument to `force`");
    }
    return super.apply(evaluator, env);
  }
}
