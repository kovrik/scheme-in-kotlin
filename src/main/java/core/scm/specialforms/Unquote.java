package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.Symbol;

import java.util.List;

public enum Unquote implements ISpecialForm {
  UNQUOTE;

  public static final Symbol UNQUOTE_SYMBOL = Symbol.intern(UNQUOTE.toString());

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    // Implemented in quasiquote
    throw IllegalSyntaxException.of(toString(), expression, "not in quasiquote");
  }

  @Override
  public String toString() {
    return "unquote";
  }
}
