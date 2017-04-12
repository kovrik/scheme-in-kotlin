package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.SCMCons;
import core.scm.SCMDelay;

import java.util.List;

/* Syntax:
 * (delay <expression>)
 */
public enum Delay implements ISpecialForm {
  DELAY;

  @Override
  public SCMDelay eval(List<Object> expression, Environment env, Evaluator evaluator) {
    if (expression.size() < 2) {
      throw IllegalSyntaxException.of(toString(), expression);
    }
    Object expr;
    if (expression.size() > 2) {
      SCMCons list = SCMCons.list(Begin.BEGIN);
      list.addAll(expression.subList(1, expression.size()));
      expr = list;
    } else {
      expr = expression.get(1);
    }
    return new SCMDelay(expr, env, evaluator);
  }

  @Override
  public String toString() {
    return "delay";
  }
}
