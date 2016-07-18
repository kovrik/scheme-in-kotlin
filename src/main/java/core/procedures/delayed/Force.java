package core.procedures.delayed;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;
import core.writer.Writer;

public class Force extends SCMProcedure {

  private static final SCMSymbol PROMISE = new SCMSymbol("promise");
  private static final SCMCons FORCE = SCMCons.list(PROMISE);

  public Force() {
    /* (lambda (p) (p)) */
    super("force", FORCE, FORCE);
  }

  @Override
  public Object apply(IEvaluator evaluator, IEnvironment env) {
    Object promise = env.get(PROMISE);
    if (!(promise instanceof SCMPromise)) {
      throw new IllegalArgumentException("Wrong argument type. Expected: Promise, actual: " + Writer.write(promise));
    }
    return super.apply(evaluator, env);
  }
}
