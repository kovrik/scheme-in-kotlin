package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.*;

import java.util.List;

import static core.scm.SCMUnspecified.UNSPECIFIED;

/* Syntax:
 * (if <test> <consequent> <alternate>)
 * (if <test> <consequent>)
 */
public enum If implements ISpecialForm, ISCMClass {
  IF;

  private static final String syntax = "if";

  @Override
  public Object eval(List<Object> expression, IEnvironment env, IEvaluator evaluator) {
    int size = expression.size();
    if (size < 3 || size > 4) {
      throw IllegalSyntaxException.of(syntax, expression, String.format("has %s parts after keyword", size - 1));
    }
    Object test = expression.get(1);
    Object consequence = expression.get(2);
    if (SCMBoolean.valueOf(evaluator.eval(test, env))) {
      return new SCMTailCall(consequence, env);
    } else {
      if (size < 4) {
        /* Here we make `if` behave like `when` if no alternative is specified.
         * Another option is to throw an exception (if: missing an "else" expression) */
        return UNSPECIFIED;
      }
      Object alternative = expression.get(3);
      return new SCMTailCall(alternative, env);
    }
  }

  @Override
  public String toString() {
    return syntax;
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.SPECIALFORM;
  }
}
