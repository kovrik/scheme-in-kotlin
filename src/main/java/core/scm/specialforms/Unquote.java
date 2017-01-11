package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.SCMSymbol;

import java.util.List;

public enum Unquote implements ISpecialForm {
  UNQUOTE;

  private static final String syntax = "unquote";

  public static final SCMSymbol UNQUOTE_SYMBOL = new SCMSymbol(syntax);

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
