package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.ISCMClass;
import core.scm.SCMClass;
import core.scm.SCMSymbol;

import java.util.List;

/* Literal expressions
 * Syntax:
 * (quote <datum>)
 * '<datum>
 * <constant>
 */
public class Quote implements ISpecialForm, ISCMClass {

  public static final Quote QUOTE = new Quote();

  private final String syntax = "quote";
  private final SCMSymbol symbol = new SCMSymbol(this.syntax);

  private Quote() {}

  @Override
  public Object eval(List<Object> expression, IEnvironment env, IEvaluator evaluator) {
    return expression.get(1);
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
