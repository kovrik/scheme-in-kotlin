package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.SCMSymbol;

import java.util.List;

/* Literal expressions
 * Syntax:
 * (quote <datum>)
 * '<datum>
 * <constant>
 */
public enum Quote implements ISpecialForm {
  QUOTE;

  private static final String syntax = "quote";

  public static final SCMSymbol QUOTE_SYMBOL = new SCMSymbol(syntax);

  @Override
  public Object eval(List<Object> expression, IEnvironment env, IEvaluator evaluator) {
    return expression.get(1);
  }

  @Override
  public String toString() {
    return syntax;
  }
}
