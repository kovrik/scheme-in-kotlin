package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.*;

import java.util.List;

/* Syntax:
 * (and <test1> ...)
 */
public enum And implements ISpecialForm, ISCMClass {
  AND;

  @Override
  public Object eval(List<Object> expression, IEnvironment env, IEvaluator evaluator) {
    Object result = SCMBoolean.TRUE;
    if (expression.size() > 1) {
      for (int i = 1; i < expression.size() - 1; i++) {
        result = evaluator.eval(expression.get(i), env);
        if (!SCMBoolean.valueOf(result)) {
          return result;
        }
      }
      result = new SCMTailCall(expression.get(expression.size() - 1), env);
    }
    return result;
  }

  @Override
  public String toString() {
    return "and";
  }
}
