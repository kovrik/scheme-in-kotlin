package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.Thunk;
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
    if (size != 4) {
      throw IllegalSyntaxException.of(toString(), expression, String.format("has %s parts after keyword", size - 1));
    }
    return Utils.INSTANCE.toBoolean(evaluator.eval(expression.get(1), env)) ?
           new Thunk(expression.get(2), env) :
           new Thunk(expression.get(3), env);
  }

  @Override
  public String toString() {
    return "if";
  }
}
