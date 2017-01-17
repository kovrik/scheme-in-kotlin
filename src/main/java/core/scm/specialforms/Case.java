package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.procedures.equivalence.Eqv;
import core.scm.SCMSymbol;
import core.scm.SCMThunk;

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

  private static final SCMSymbol ELSE = SCMSymbol.of("else");

  private static final String syntax = "case";

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    /* Save string representation of expression before evaluation */
    String exprString = expression.toString();
    if (expression.size() <= 1) {
      throw IllegalSyntaxException.of(syntax, exprString, "source expression failed to match any pattern");
    }
    Object key = evaluator.eval(expression.get(1), env);
    for (int i = 2; i < expression.size(); i++) {
      Object node = expression.get(i);
      if (!(node instanceof List)) {
        throw IllegalSyntaxException.of(syntax, exprString, "invalid clause in subform");
      }
      List<Object> subform = (List)node;
      Object datum = subform.get(0);
      if (ELSE.equals(datum)) {
        if (i == expression.size() - 1) {
          for (int s = 1; s < subform.size() - 1; s++) {
            evaluator.eval(subform.get(s), env);
          }
          return new SCMThunk(subform.get(subform.size() - 1), env);
        }
        throw IllegalSyntaxException.of(syntax, exprString, "else must be the last clause in subform");
      }
      if (!(datum instanceof List)) {
        throw IllegalSyntaxException.of(syntax, exprString, "invalid clause in subform");
      }
      for (Object n : ((List)datum)) {
        if (Eqv.eqv(key, n)) {
          for (int s = 1; i < subform.size() - 1; i++) {
            evaluator.eval(subform.get(s), env);
          }
          return new SCMThunk(subform.get(subform.size() - 1), env);
        }
      }
    }
    return UNSPECIFIED;
  }

  @Override
  public String toString() {
    return syntax;
  }
}
