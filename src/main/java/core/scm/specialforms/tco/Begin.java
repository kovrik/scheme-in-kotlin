package core.scm.specialforms.tco;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.ISCMClass;
import core.scm.SCMClass;
import core.scm.SCMSymbol;
import core.scm.specialforms.ISpecialForm;

import java.util.List;

/* Syntax:
 * (begin <expression1> <expression2> ...)
 */
public class Begin implements ISpecialForm, ISCMClass {

  public static final Begin BEGIN = new Begin();

  private final String syntax = "begin";
  private final SCMSymbol symbol = new SCMSymbol(this.syntax);

  private Begin() {}

  @Override
  public Object eval(List<Object> expression, IEnvironment env, IEvaluator evaluator) {
    for (int i = 1; i < expression.size() - 1; i++) {
      evaluator.eval(expression.get(i), env);
    }
    return evaluator.evalp(expression.get(expression.size() - 1), env);
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
