package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.ISCMClass;
import core.scm.SCMTailCall;

import java.util.List;

/* Syntax:
 * (begin <expression1> <expression2> ...)
 */
public enum Begin implements ISpecialForm, ISCMClass {
  BEGIN;

  @Override
  public Object eval(List<Object> expression, IEnvironment env, IEvaluator evaluator) {
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
