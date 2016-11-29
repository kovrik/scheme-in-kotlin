package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.ISCMClass;
import core.scm.SCMClass;
import core.scm.SCMSymbol;
import core.scm.SCMUnspecified;

import java.util.List;

import static core.scm.SCMUnspecified.UNSPECIFIED;

/* Syntax:
 * (set! <variable> <expression>)
 */
public class Set implements ISpecialForm, ISCMClass {

  public static final Set SET = new Set();

  private final String syntax = "set!";
  private final SCMSymbol symbol = new SCMSymbol(this.syntax);

  private Set() {}

  @Override
  public SCMUnspecified eval(List<Object> expression, IEnvironment env, IEvaluator evaluator) {
    if (expression.size() != 3) {
      throw IllegalSyntaxException.of(syntax, expression, String.format("has %s parts after keyword", expression.size() - 1));
    }
    Object identifier = expression.get(1);
    if (!(identifier instanceof SCMSymbol)) {
      throw IllegalSyntaxException.of(syntax, expression, String.format("not an identifier: `%s`", identifier));
    }
    env.findAndPut(identifier, evaluator.eval(expression.get(2), env));
    return UNSPECIFIED;
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
