package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.evaluator.Reflector;
import core.exceptions.IllegalSyntaxException;

import java.util.List;

public enum New implements ISpecialForm {
  NEW;

  private final Reflector reflector = new Reflector();

  /* Wrapper to avoid downcast */
  public static class NewInstanceResult {

    private Object instance;

    private NewInstanceResult(Object instance) {
      this.instance = instance;
    }

    public Object getInstance() {
      return instance;
    }
  }

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    if (expression.size() < 2) {
      throw IllegalSyntaxException.of(toString(), expression);
    }
//    if (!SCMClass.checkType(expression.get(1), String.class)) {
//      throw new WrongTypeException(toString(), "String", expression.get(1));
//    }
    String clazz = expression.get(1).toString();

    /* Eval args */
    Object[] args = new Object[expression.size() - 2];
    for (int i = 0; i < args.length; i++) {
      args[i] = evaluator.eval(expression.get(i + 2), env);
    }
    return new NewInstanceResult(reflector.newInstance(clazz, args));
  }

  @Override
  public String toString() {
    return "new";
  }
}
