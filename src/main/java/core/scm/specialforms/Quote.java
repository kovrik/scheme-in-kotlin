package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.scm.Cons;
import core.scm.Symbol;

import java.util.List;

/* Literal expressions
 * Syntax:
 * (quote <datum>)
 * '<datum>
 * <constant>
 */
public enum Quote implements ISpecialForm {
  QUOTE;

  public static final Symbol QUOTE_SYMBOL = Symbol.intern(QUOTE.toString());

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    return expression.get(1);
  }

  @Override
  public String toString() {
    return "quote";
  }

  public static List quote(Object obj) {
    return Cons.list(QUOTE, obj);
  }
}
