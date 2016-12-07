package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.*;

import java.util.List;

/* Syntax:
 * (cond <clause1> <clause2> ...)
 *
 * <clause>: (<test> <expression1> ...)
 *
 * Last clause may be:
 * (else <expression1> <expression2> ...)
 */
public enum Cond implements ISpecialForm, ISCMClass {
  COND;

  private static final SCMSymbol ELSE = new SCMSymbol("else");

  private static final String syntax = "cond";

  @Override
  public Object eval(List<Object> expression, IEnvironment env, IEvaluator evaluator) {
    for (int i = 1; i < expression.size(); i++) {
      Object node = expression.get(i);
      if (!(node instanceof List)) {
        throw IllegalSyntaxException.of(syntax, expression, "invalid clause in subform");
      }
      List<Object> subform = (List)node;
      Object clause = subform.get(0);
      if (ELSE.equals(clause)) {
        if (i == expression.size() - 1) {
          for (int s = 1; i < subform.size() - 1; i++) {
            evaluator.eval(subform.get(s), env);
          }
          return new SCMTailCall(subform.get(subform.size() - 1), env);
        }
        throw IllegalSyntaxException.of(syntax, expression, "else must be the last clause in subform");
      }
      if (SCMBoolean.valueOf(evaluator.eval(clause, env))) {
        for (int s = 1; s < subform.size() - 1; s++) {
          evaluator.eval(subform.get(s), env);
        }
        return new SCMTailCall(subform.get(subform.size() - 1), env);
      }
    }
    return SCMUnspecified.UNSPECIFIED;
  }

  @Override
  public String toString() {
    return syntax;
  }
}
