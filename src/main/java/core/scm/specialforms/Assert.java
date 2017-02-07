package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.SCMBoolean;
import core.scm.SCMError;
import core.scm.SCMMutableString;

import java.util.List;

public enum Assert implements ISpecialForm {
  ASSERT;

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    if (expression.size() < 2 || expression.size() > 3) {
      throw IllegalSyntaxException.of(toString(), expression);
    }
    Object result = evaluator.eval(expression.get(1), env);
    if (!SCMBoolean.toBoolean(result)) {
      String message = "";
      if (expression.size() == 3) {
        if (!(expression.get(2) instanceof String) && !(expression.get(2) instanceof SCMMutableString)) {
          throw IllegalSyntaxException.of(toString(), expression);
        }
        message = expression.get(2).toString();
      }
      throw new SCMError(message);
    }
    return Boolean.TRUE;
  }

  @Override
  public String toString() {
    return "assert";
  }
}
