package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.SCMPromise;

import java.util.List;

/* Syntax:
 * (delay <expression>)
 */
public enum Delay implements ISpecialForm {
  DELAY;

  @Override
  public SCMPromise eval(List<Object> expression, Environment env, Evaluator evaluator) {
    if (expression.size() < 2) {
      throw IllegalSyntaxException.of(toString(), expression);
    }
    return new SCMPromise(expression.get(1));
  }

  @Override
  public String toString() {
    return "delay";
  }
}
