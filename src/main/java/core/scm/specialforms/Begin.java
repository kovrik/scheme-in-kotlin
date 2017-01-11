package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.scm.SCMTailCall;

import java.util.List;

/* Syntax:
 * (begin <expression1> <expression2> ...)
 */
public enum Begin implements ISpecialForm {
  BEGIN;

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    for (int i = 1; i < expression.size() - 1; i++) {
      evaluator.eval(expression.get(i), env);
    }
    return new SCMTailCall(expression.get(expression.size() - 1), env);
  }

  @Override
  public String toString() {
    return "begin";
  }
}
