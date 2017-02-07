package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.scm.SCMThunk;
import core.scm.SCMConstant;

import java.util.List;
import java.util.stream.IntStream;

/* Syntax:
 * (begin <expression1> <expression2> ...)
 */
public enum Begin implements ISpecialForm {
  BEGIN;

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    if (expression.size() == 0) {
      return SCMConstant.UNSPECIFIED;
    }
    // FIXME
    if (expression.size() == 1 && BEGIN == (expression.get(0))) {
      return SCMConstant.UNSPECIFIED;
    }
    IntStream.range(1, expression.size() - 1).forEach(i -> evaluator.eval(expression.get(i), env));
    return new SCMThunk(expression.get(expression.size() - 1), env);
  }

  @Override
  public String toString() {
    return "begin";
  }
}
