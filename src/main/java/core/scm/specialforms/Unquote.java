package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.ISCMClass;
import core.scm.SCMClass;
import core.scm.SCMSymbol;

import java.util.List;

public class Unquote implements ISpecialForm, ISCMClass {

  public static final Unquote UNQUOTE = new Unquote();

  private final String syntax = "unquote";
  private final SCMSymbol symbol = new SCMSymbol(this.syntax);

  private Unquote() {}

  @Override
  public Object eval(List<Object> expression, IEnvironment env, IEvaluator evaluator) {
    // Implemented in quasiquote
//    if (!evaluator.isInQuasiquote()) {
//      throw new IllegalSyntaxException("unquote: not in quasiquote");
//    }
    if (expression.size() != 2) {
      throw new IllegalSyntaxException("unquote: expects exactly one expression");
    }
    return evaluator.eval(expression, env);
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
