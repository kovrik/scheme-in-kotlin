package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.scm.SCMConstant;

import java.util.List;

/**
 * Comment Special Form:
 *
 * Ignores body and returns nothing
 *
 * Syntax:
 * (comment <expression1> ... <expression n>)
 */
public enum Comment implements ISpecialForm {
  COMMENT;

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    return SCMConstant.UNSPECIFIED;
  }

  @Override
  public String toString() {
    return "comment";
  }
}
