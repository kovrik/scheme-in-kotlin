package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.SCMSymbol;

import java.util.List;

public enum UnquoteSplicing implements ISpecialForm {
  UNQUOTE_SPLICING;

  private static final String syntax = "unquote-splicing";

  public static final SCMSymbol UNQUOTE_SPLICING_SYMBOL = new SCMSymbol(syntax);

  @Override
  public Object eval(List<Object> expression, IEnvironment env, IEvaluator evaluator) {
    // Implemented in quasiquote
    throw IllegalSyntaxException.of(syntax, expression, "not in quasiquote");
  }

  @Override
  public String toString() {
    return syntax;
  }
}
