package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.SCMSymbol;

import java.util.List;

public enum UnquoteSplicing implements ISpecialForm {
  UNQUOTE_SPLICING;

  private static final String syntax = "unquote-splicing";

  public static final SCMSymbol UNQUOTE_SPLICING_SYMBOL = new SCMSymbol(syntax);

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    // Implemented in quasiquote
    throw IllegalSyntaxException.of(syntax, expression, "not in quasiquote");
  }

  @Override
  public String toString() {
    return syntax;
  }
}
