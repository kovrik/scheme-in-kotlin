package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.scm.Thunk;
import core.utils.Utils;

import java.util.List;

/* Syntax:
 * (and <test1> ...)
 */
public enum And implements ISpecialForm {
  AND;

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    Object result = Boolean.TRUE;
    if (expression.size() > 1) {
      for (int i = 1; i < expression.size() - 1; i++) {
        result = evaluator.eval(expression.get(i), env);
        if (!Utils.INSTANCE.toBoolean(result)) {
          return result;
        }
      }
      result = new Thunk(expression.get(expression.size() - 1), env);
    }
    return result;
  }

  @Override
  public String toString() {
    return "and";
  }
}
