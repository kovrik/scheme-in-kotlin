package core.procedures.delayed;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.ReentrantPromiseException;
import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMPromise;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {SCMPromise.class})
public final class Force extends AFn {

  @Override
  public String getName() {
    return "force";
  }

  @Override
  public Object apply1(Object arg) {
    throw new UnsupportedOperationException(getName() + ": must be evaluated in Evaluator!");
  }

  /* Actual force implementation */
  public Object force(SCMPromise p, Environment env, Evaluator evaluator) {
    switch (p.getState()) {
      case FULFILLED: return p.getValue();
      case REJECTED : throw (RuntimeException) p.getValue();
      case FORCED:    throw new ReentrantPromiseException(p);
      default: {
        p.setState(SCMPromise.State.FORCED);
        try {
          /* Evaluate the body */
          Object result = evaluator.eval(p.getExpr(), env);
          /* Mark Promise as FULFILLED */
          p.setState(SCMPromise.State.FULFILLED);
          /* Memoize the result */
          p.setValue(result);
          return result;
        } catch (Exception e) {
          /* Mark Promise as REJECTED */
          p.setState(SCMPromise.State.REJECTED);
          /* Memoize the result */
          p.setValue(e);
          throw e;
        }
      }
    }
  }
}
