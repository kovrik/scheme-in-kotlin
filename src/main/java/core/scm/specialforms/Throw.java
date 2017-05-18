package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.exceptions.ThrowableWrapper;
import core.exceptions.WrongTypeException;

import java.util.List;

public enum Throw implements ISpecialForm {
  THROW;

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    if (expression.size() < 2) {
      throw IllegalSyntaxException.Companion.of(toString(), expression);
    }
    Object obj = evaluator.eval(expression.get(1), env);
    if (!(obj instanceof Throwable)) {
      throw new WrongTypeException(toString(), "Throwable", obj);
    }
    throw new ThrowableWrapper((Throwable) obj);
  }

  @Override
  public String toString() {
    return "throw";
  }
}
