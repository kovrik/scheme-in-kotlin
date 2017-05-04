package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.exceptions.WrongTypeException;
import core.procedures.IFn;
import core.scm.Cons;

import java.util.List;

public enum DynamicWind implements ISpecialForm {
  DYNAMIC_WIND;

  @Override
  public String toString() {
    return "dynamic-wind";
  }

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    int size = expression.size();
    if (size != 4) {
      throw IllegalSyntaxException.of(toString(), expression, String.format("has %s parts after keyword", size - 1));
    }
    Object pre = evaluator.eval(expression.get(1), env);
    if (!(pre instanceof IFn)) {
      throw new WrongTypeException(toString(), "Procedure", pre);
    }
    Object value = evaluator.eval(expression.get(2), env);
    if (!(value instanceof IFn)) {
      throw new WrongTypeException(toString(), "Procedure", value);
    }
    Object post = evaluator.eval(expression.get(3), env);
    if (!(post instanceof IFn)) {
      throw new WrongTypeException(toString(), "Procedure", post);
    }
    /* Evaluate before-thunk first */
    evaluator.eval(Cons.list(pre), env);
    try {
      /* Evaluate and return value-thunk */
      return evaluator.eval(Cons.list(value), env);
    } finally {
      /* Finally, evaluate post-thunk */
      evaluator.eval(Cons.list(post), env);
    }
  }
}
