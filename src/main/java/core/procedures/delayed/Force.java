package core.procedures.delayed;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMPromise;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {SCMPromise.class})
public class Force extends AFn {

  @Override
  public String getName() {
    return "force";
  }

  @Override
  public Object apply1(Object arg) {
    SCMPromise promise = (SCMPromise) arg;
    switch (promise.getState()) {
      case FULFILLED: return promise.getResult();
      case REJECTED:  throw (RuntimeException)promise.getResult();
      default:        return promise;
    }
  }
}
