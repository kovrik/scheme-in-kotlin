package core.procedures.continuations;

import core.procedures.AFn;
import core.procedures.IFn;
import core.scm.FnArgs;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {IFn.class})
public class CallCC extends AFn {

  @Override
  public boolean isPure() {
    return false;
  }

  @Override
  public String getName() {
    return "call-with-current-continuation";
  }

  @Override
  public Object apply(Object... args) {
    throw new UnsupportedOperationException("Must be evaluated in Evaluator!");
  }
}
