package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.Thunk;
import core.scm.Void;
import core.utils.Utils;

import java.util.List;

/* Syntax:
 * (if <test> <consequent> <alternate>)
 * (if <test> <consequent>)
 */
public enum If implements ISpecialForm {
  IF;

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    int size = expression.size();
    if (size < 3 || size > 4) {
      throw IllegalSyntaxException.of(toString(), expression, String.format("has %s parts after keyword", size - 1));
    }
    Object test = expression.get(1);
    if (Utils.toBoolean(evaluator.eval(test, env))) {
      return new Thunk(expression.get(2), env);
    } else {
      if (size < 4) {
        /* Here we make `if` behave like `when` if no alternative is specified.
         * Another option is to throw an exception (if: missing an "else" expression) */
        return Void.VOID;
      }
      return new Thunk(expression.get(3), env);
    }
  }

  @Override
  public String toString() {
    return "if";
  }
}
