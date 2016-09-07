package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.scm.ISCMClass;
import core.scm.SCMBoolean;
import core.scm.SCMClass;
import core.scm.SCMSymbol;

import java.util.List;

/* Syntax:
 * (or <test1> ...)
 */
@Deprecated
public class Or implements ISpecialForm, ISCMClass {

  public static final Or OR = new Or();

  private final String syntax = "or";
  private final SCMSymbol symbol = new SCMSymbol(this.syntax);

  private Or() {}

  @Override
  public Object eval(List<Object> expression, IEnvironment env, IEvaluator evaluator) {
    Object eval = SCMBoolean.FALSE;
    if (expression.size() > 1) {
      for (int i = 1; i < expression.size(); i++) {
        eval = evaluator.eval(expression.get(i), env);
        if (SCMBoolean.valueOf(eval)) {
          return eval;
        }
      }
    }
    return eval;
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
