package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.ISCMClass;
import core.scm.SCMClass;
import core.scm.SCMSymbol;

import java.util.List;

public enum Unquote implements ISpecialForm, ISCMClass {
  UNQUOTE;

  private static final String syntax = "unquote";

  public static final SCMSymbol UNQUOTE_SYMBOL = new SCMSymbol(syntax);

  @Override
  public Object eval(List<Object> expression, IEnvironment env, IEvaluator evaluator) {
    // Implemented in quasiquote
    throw IllegalSyntaxException.of(syntax, expression, "not in quasiquote");
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
