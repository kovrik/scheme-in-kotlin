package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.scm.Thunk;
import core.utils.Utils;

import java.util.List;

/* Syntax:
 * (or <test1> ...)
 */
public enum Or implements ISpecialForm {
  OR;

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    Object result = Boolean.FALSE;
    if (expression.size() > 1) {
      for (int i = 1; i < expression.size() - 1; i++) {
        result = evaluator.eval(expression.get(i), env);
        if (Utils.toBoolean(result)) {
          return result;
        }
      }
      result = new Thunk(expression.get(expression.size() - 1), env);
    }
    return result;
  }

  @Override
  public String toString() {
    return "or";
  }
}
