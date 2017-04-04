package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.evaluator.Reflector;
import core.exceptions.IllegalSyntaxException;

import java.util.List;

public enum New implements ISpecialForm {
  NEW;

  private final Reflector reflector = new Reflector();

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    if (expression.size() < 2) {
      throw IllegalSyntaxException.of(toString(), expression);
    }
    Object[] arguments = null;
    if (expression.size() > 2) {
      arguments = expression.subList(2, expression.size()).toArray();
    }
//    if (!SCMClass.checkType(expression.get(1), String.class)) {
//      throw new WrongTypeException(toString(), "String", expression.get(1));
//    }
    String clazz = expression.get(1).toString();
    return reflector.newInstance(clazz, arguments);
  }

  @Override
  public String toString() {
    return "new";
  }
}
