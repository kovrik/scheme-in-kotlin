package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.WrongTypeException;
import core.procedures.IFn;
import core.procedures.continuations.CalledContinuation;
import core.procedures.continuations.Continuation;
import core.scm.Cons;

import java.util.List;

public enum CallCC implements ISpecialForm {
  CALL_CC {
    @Override
    public String toString() {
      return "call/cc";
    }
  },
  CALL_WITH_CURRENT_CONTINUATION {
    @Override
    public String toString() {
      return "call-with-current-continuation";
    }
  };

  /* Actual call-with-current-continuation */
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    Object proc = evaluator.eval(expression.get(1), env);
    if (!(proc instanceof IFn)) {
      throw new WrongTypeException(toString(), "Procedure", proc);
    }
    Continuation cont = new Continuation();
    try {
      /* Pass Continuation to the Procedure: (proc cont) */
      return evaluator.eval(Cons.list(proc, cont), env);
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
