package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.scm.Void;

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
  public Void eval(List<Object> expression, Environment env, Evaluator evaluator) {
    return Void.VOID;
  }

  @Override
  public String toString() {
    return "comment";
  }
}
