package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.SCMSymbol;
import core.scm.SCMUnspecified;

import java.util.List;

import static core.scm.SCMUnspecified.UNSPECIFIED;

/* Syntax:
 * (set! <variable> <expression>)
 */
public enum Set implements ISpecialForm {
  SET;

  @Override
  public SCMUnspecified eval(List<Object> expression, Environment env, Evaluator evaluator) {
    if (expression.size() != 3) {
      throw IllegalSyntaxException.of(toString(), expression, String.format("has %s parts after keyword", expression.size() - 1));
    }
    Object identifier = expression.get(1);
    if (!(identifier instanceof SCMSymbol)) {
      throw IllegalSyntaxException.of(toString(), expression, String.format("not an identifier: `%s`", identifier));
    }
    env.findAndPut(identifier, evaluator.eval(expression.get(2), env));
    return UNSPECIFIED;
  }

  @Override
  public String toString() {
    return "set!";
  }
}
