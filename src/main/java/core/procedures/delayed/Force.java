package core.procedures.delayed;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.ReentrantDelayException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMFuture;
import core.scm.SCMDelay;
import core.scm.SCMPromise;

public final class Force extends AFn {

  // FIXME Get rid of this public static instance
  public static final Force FORCE = new Force();

  public Force() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{SCMDelay.class}));
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
  public Object force(Object delay, Environment env, Evaluator evaluator) {
    if (delay instanceof SCMPromise) {
      return delay;
    }
    if (!(delay instanceof SCMDelay)) {
      throw new WrongTypeException(getName(), "Promise or Future or Delay", delay);
    }
    SCMDelay p  = (SCMDelay)delay;
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
        /* Not allowed to force delays multiple times! */
        throw new ReentrantDelayException(p);
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
  // - always block (even on promise and delay, but always by current thread)
  // - fix synchronization? use locks?
  private Object forceDelayed(final SCMDelay p, Environment env, Evaluator evaluator) {
    p.setState(SCMDelay.State.FORCED);
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
      /* Mark delay as FULFILLED */
      p.setState(SCMDelay.State.FULFILLED);
      /* Memoize the result */
      p.setValue(result);
      return result;
    } catch (Exception e) {
      /* Mark delay as REJECTED */
      p.setState(SCMDelay.State.REJECTED);
      /* Memoize the result */
      p.setValue(e);
      throw e;
    }
  }
}
