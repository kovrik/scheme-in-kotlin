package core.procedures.delayed;

import core.exceptions.ArityException;
import core.exceptions.ReentrantPromiseException;
import core.procedures.AFn;
import core.scm.SCMPromise;

public class Force extends AFn {

  @Override
  public Object invoke(Object... args) {
    if (args.length != 1) {
      throw new ArityException(args.length, 1, "force");
    }
    Object o = args[0];
    if (!(o instanceof SCMPromise)) {
      return o;
    }
    SCMPromise promise = (SCMPromise) o;
    if (promise.getState() == SCMPromise.State.FULFILLED) {
      return promise.getResult();
    }
    if (promise.getState() == SCMPromise.State.REJECTED) {
      throw (RuntimeException)promise.getResult();
    }
    if (promise.getState() == SCMPromise.State.FORCED) {
      throw new ReentrantPromiseException(promise);
    }
    /* Force Promise evaluation */
    promise.setState(SCMPromise.State.FORCED);
    return promise;
  }
}
