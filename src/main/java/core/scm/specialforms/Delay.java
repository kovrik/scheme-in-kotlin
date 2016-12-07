package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.ISCMClass;
import core.scm.SCMPromise;

import java.util.List;

/* Syntax:
 * (delay <expression>)
 */
public enum Delay implements ISpecialForm, ISCMClass {
  DELAY;

  private static final String syntax = "delay";

  @Override
  public SCMPromise eval(List<Object> expression, IEnvironment env, IEvaluator evaluator) {
    if (expression.size() < 2) {
      throw IllegalSyntaxException.of(syntax, expression);
    }
    return new SCMPromise(expression.get(1));
  }

  @Override
  public String toString() {
    return syntax;
  }
}
