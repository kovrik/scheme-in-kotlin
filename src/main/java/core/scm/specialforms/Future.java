package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.SCMCons;
import core.scm.SCMFuture;
import core.scm.SCMDelay;

import java.util.List;

/* Syntax:
 * (future <expression>)
 */
public enum Future implements ISpecialForm {
  FUTURE;

  @Override
  public SCMDelay eval(List<Object> expression, Environment env, Evaluator evaluator) {
    if (expression.size() < 2) {
      throw IllegalSyntaxException.of(toString(), expression);
    }
    if (expression.size() > 2) {
      SCMCons list = SCMCons.list(Begin.BEGIN);
      list.addAll(expression.subList(1, expression.size()));
      return new SCMFuture(list);
    }
    return new SCMFuture(expression.get(1));
  }

  @Override
  public String toString() {
    return "future";
  }
}
