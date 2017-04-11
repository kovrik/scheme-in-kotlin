package core.procedures.delayed;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.ReentrantPromiseException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMFuture;
import core.scm.SCMPromise;

public final class Force extends AFn {

  // FIXME Get rid of this public static instance
  public static final Force FORCE = new Force();

  public Force() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{SCMPromise.class}));
  }

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
      case FORCED:    {
        if (p instanceof SCMFuture) {
          // FIXME This is just a workaround, make it the right way!!!
          /* Computation is not finished yet, so block until it is done */
          synchronized (p) {
            return p.getValue();
          }
        }
        /* Not allowed to force promises multiple times! */
        throw new ReentrantPromiseException(p);
      }
      default: {
        if (p instanceof SCMFuture) {
          /* Run Future in a separate thread */
          new Thread(() -> forceDelayed(p, env, evaluator)).start();
          return p;
        }
        return forceDelayed(p, env, evaluator);
      }
    }
  }

  // TODO Make it right:
  // - always block (even on promise, but always by current thread)
  // - make promises deliverable
  // - fix synchronization? use locks?
  private Object forceDelayed(final SCMPromise p, Environment env, Evaluator evaluator) {
    p.setState(SCMPromise.State.FORCED);
    try {
      /* Evaluate the body */
      Object result;
      if (p instanceof SCMFuture) {
        /* Block on future until computation is complete */
        synchronized (p) {
          result = evaluator.eval(p.getExpr(), env);
        }
      } else {
        result = evaluator.eval(p.getExpr(), env);
      }
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
