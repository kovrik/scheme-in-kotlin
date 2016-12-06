package core.procedures.delayed;

import core.exceptions.ReentrantPromiseException;
import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMPromise;

@FnArgs(args = {SCMPromise.class})
public class Force extends AFn {

  @Override
  public String getName() {
    return "force";
  }

  @Override
  public Object invoke(Object... args) {
    SCMPromise promise = (SCMPromise) args[0];
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
