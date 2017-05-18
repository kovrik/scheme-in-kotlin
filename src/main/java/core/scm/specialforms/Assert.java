package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.MutableString;
import core.utils.Utils;

import java.util.List;

public enum Assert implements ISpecialForm {
  ASSERT;

  private static final StackTraceElement[] EMPTY = new StackTraceElement[0];

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    if (expression.size() < 2 || expression.size() > 3) {
      throw IllegalSyntaxException.of(toString(), expression);
    }
    Object result = evaluator.eval(expression.get(1), env);
    if (!Utils.INSTANCE.toBoolean(result)) {
      String message = "";
      if (expression.size() == 3) {
        if (!(expression.get(2) instanceof String) && !(expression.get(2) instanceof MutableString)) {
          throw IllegalSyntaxException.of(toString(), expression);
        }
        message = ": " + expression.get(2).toString();
      }
      AssertionError assertionError = new AssertionError("assert failed" + message);
      assertionError.setStackTrace(EMPTY);
      throw assertionError;
    }
    return Boolean.TRUE;
  }

  @Override
  public String toString() {
    return "assert";
  }
}
