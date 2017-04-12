package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.SCMCons;
import core.scm.SCMFuture;

import java.util.List;

/* Syntax:
 * (future <expression>)
 */
public enum Future implements ISpecialForm {
  FUTURE;

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    if (expression.size() < 2) {
      throw IllegalSyntaxException.of(toString(), expression);
    }
    SCMFuture future;
    if (expression.size() > 2) {
      SCMCons list = SCMCons.list(Begin.BEGIN);
      list.addAll(expression.subList(1, expression.size()));
      future = new SCMFuture(list, env, evaluator);
    } else {
      future = new SCMFuture(expression.get(1), env, evaluator);
    }
    Evaluator.executor.submit(future);
    return future;
  }

  @Override
  public String toString() {
    return "future";
  }
}
