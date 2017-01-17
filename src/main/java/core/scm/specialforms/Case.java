package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.procedures.equivalence.Eqv;

import java.util.List;

import static core.scm.SCMUnspecified.UNSPECIFIED;

/* Syntax:
 * (case <key> <clause1> <clause2> ...)
 *
 * <clause>: ((<datum1> ...) <expression1> <expression2> ...)
 *
 * Last clause may be:
 * (else <expression1> <expression2> ...)
 */
public enum Case implements ISpecialForm {
  CASE;

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    /* Save string representation of expression before evaluation */
    String exprString = expression.toString();
    if (expression.size() <= 1) {
      throw IllegalSyntaxException.of(toString(), exprString, "source expression failed to match any pattern");
    }
    Object key = evaluator.eval(expression.get(1), env);
    for (int i = 2; i < expression.size(); i++) {
      Object node = expression.get(i);
      if (!(node instanceof List)) {
        throw IllegalSyntaxException.of(toString(), exprString, "invalid clause in subform");
      }
      List<Object> subform = (List)node;
      Object datum = subform.get(0);
      if (Else.ELSE_SYMBOL.equals(datum)) {
        if (i != expression.size() - 1) {
          throw IllegalSyntaxException.of(toString(), exprString, "else must be the last clause in subform");
        }
        return Begin.BEGIN.eval(subform, env, evaluator);
      }
      if (!(datum instanceof List)) {
        throw IllegalSyntaxException.of(toString(), exprString, "invalid clause in subform");
      }
      for (Object n : ((List)datum)) {
        if (Eqv.eqv(key, n)) {
          return Begin.BEGIN.eval(subform, env, evaluator);
        }
      }
    }
    return UNSPECIFIED;
  }

  @Override
  public String toString() {
    return "case";
  }
}
