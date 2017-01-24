package core.procedures.continuations;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.procedures.AFn;
import core.procedures.IFn;
import core.scm.FnArgs;
import core.scm.SCMCons;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {IFn.class})
public final class CallCC extends AFn {

  @Override
  public boolean isPure() {
    return false;
  }

  @Override
  public String getName() {
    return "call-with-current-continuation";
  }

  @Override
  public Object apply1(Object arg) {
    throw new UnsupportedOperationException(getName() + ": must be evaluated in Evaluator!");
  }

  /* Actual call-with-current-continuation */
  public Object callcc(IFn proc, Environment env, Evaluator evaluator) {
    Continuation cont = new Continuation();
    try {
      /* Pass Continuation to the Procedure: (proc cont) */
      return evaluator.eval(SCMCons.list(proc, cont), env);
    } catch (CalledContinuation ex) {
      if (ex.getContinuation() != cont) {
        /* Not our continuation, throw it further */
        throw ex;
      }
      /* Our continuation, grab and return the resulting value */
      return ex.getValue();
    } finally {
      /* One-shot continuations cannot be used more than once */
      cont.invalidate();
    }
  }
}
