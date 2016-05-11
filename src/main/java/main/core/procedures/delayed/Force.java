package main.core.procedures.delayed;

import main.core.ast.SCMList;
import main.core.ast.SCMSymbol;
import main.core.environment.IEnvironment;
import main.core.evaluator.IEvaluator;
import main.core.procedures.Procedure;

import java.util.Collections;

public class Force extends Procedure {

  private static final SCMSymbol PROMISE = new SCMSymbol("promise");

  /* (lambda (p) (p)) */
  private static final SCMList<Object> FORCE = new SCMList<Object>(Collections.singletonList((Object)new SCMSymbol("promise")));

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    Object promise = env.get(PROMISE);
    if (!(promise instanceof Promise)) {
      throw new IllegalArgumentException("Wrong type argument to `force`");
    }
    return super.apply(evaluator, env);
  }

  public Force() {
    super(FORCE, FORCE);
  }
}
