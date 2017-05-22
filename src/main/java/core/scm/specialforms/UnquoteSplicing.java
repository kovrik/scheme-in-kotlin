package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.Symbol;

import java.util.List;

public enum UnquoteSplicing implements ISpecialForm {
  UNQUOTE_SPLICING;

  public static final Symbol UNQUOTE_SPLICING_SYMBOL = Symbol.Companion.intern(UNQUOTE_SPLICING.toString());

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    // Implemented in quasiquote
    throw IllegalSyntaxException.Companion.of(toString(), expression, "not in quasiquote");
  }

  @Override
  public String toString() {
    return "unquote-splicing";
  }
}
