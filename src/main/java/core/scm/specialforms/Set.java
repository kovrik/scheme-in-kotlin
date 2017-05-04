package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.Symbol;
import core.scm.Void;

import java.util.List;

/* Syntax:
 * (set! <variable> <expression>)
 */
public enum Set implements ISpecialForm {
  SET;

  @Override
  public Void eval(List<Object> expression, Environment env, Evaluator evaluator) {
    if (expression.size() != 3) {
      throw IllegalSyntaxException.of(toString(), expression, String.format("has %s parts after keyword", expression.size() - 1));
    }
    Object identifier = expression.get(1);
    if (!(identifier instanceof Symbol)) {
      throw IllegalSyntaxException.of(toString(), expression, String.format("not an identifier: `%s`", identifier));
    }
    env.findAndPut(identifier, evaluator.eval(expression.get(2), env));
    return Void.VOID;
  }

  @Override
  public String toString() {
    return "set!";
  }
}
