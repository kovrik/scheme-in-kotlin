package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.Thunk;
import core.scm.Void;
import core.utils.Utils;

import java.util.List;

/* Syntax:
 * (unless <test> body...)
 */
public enum Unless implements ISpecialForm {
  UNLESS;

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    int size = expression.size();
    if (size < 3) {
      throw IllegalSyntaxException.Companion
        .of(toString(), expression, String.format("has %s parts after keyword", size - 1));
    }
    Object test = expression.get(1);
    if (!Utils.INSTANCE.toBoolean(evaluator.eval(test, env))) {
      for (int i = 2; i < expression.size() - 1; i++) {
        evaluator.eval(expression.get(i), env);
      }
      return new Thunk(expression.get(expression.size() - 1), env);
    }
    return Void.VOID;
  }

  @Override
  public String toString() {
    return "unless";
  }
}
