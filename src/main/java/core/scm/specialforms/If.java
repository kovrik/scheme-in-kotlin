package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.ISCMClass;
import core.scm.SCMBoolean;
import core.scm.SCMClass;
import core.scm.SCMSymbol;

import java.util.List;

import static core.scm.SCMUnspecified.UNSPECIFIED;

/* Syntax:
 * (if <test> <consequent> <alternate>)
 * (if <test> <consequent>)
 */
public class If implements ISpecialForm, ISCMClass {

  public static final If IF = new If();

  private final String syntax = "if";
  private final SCMSymbol symbol = new SCMSymbol(this.syntax);

  private If() {}

  @Override
  public Object eval(List<Object> expression, IEnvironment env, IEvaluator evaluator) {
    int size = expression.size();
    if (size < 3 || size > 4) {
      throw IllegalSyntaxException.of(syntax, expression, String.format("has %s parts after keyword", size - 1));
    }
    Object test = expression.get(1);
    Object consequence = expression.get(2);
    if (SCMBoolean.valueOf(evaluator.eval(test, env))) {
      return new TailCall(consequence, env);
    } else {
      if (size < 4) {
        /* Here we make `if` behave like `when` if no alternative is specified.
         * Another option is to throw an exception (if: missing an "else" expression) */
        return UNSPECIFIED;
      }
      Object alternative = expression.get(3);
      return new TailCall(alternative, env);
    }
  }

  public SCMSymbol symbol() {
    return symbol;
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
