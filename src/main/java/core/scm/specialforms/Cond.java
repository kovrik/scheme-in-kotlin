package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.SCMBoolean;
import core.scm.SCMConstant;

import java.util.List;

/* Syntax:
 * (cond <clause1> <clause2> ...)
 *
 * <clause>: (<test> <expression1> ...)
 *
 * Last clause may be:
 * (else <expression1> <expression2> ...)
 */
public enum Cond implements ISpecialForm {
  COND;

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    for (int i = 1; i < expression.size(); i++) {
      Object node = expression.get(i);
      if (!(node instanceof List)) {
        throw IllegalSyntaxException.of(toString(), expression, "invalid clause in subform");
      }
      List<Object> subform = (List)node;
      Object clause = subform.get(0);
      if (Else.ELSE_SYMBOL.equals(clause)) {
        if (i != expression.size() - 1) {
          throw IllegalSyntaxException.of(toString(), expression, "else must be the last clause in subform");
        }
        return Begin.BEGIN.eval(subform, env, evaluator);
      }
      if (SCMBoolean.toBoolean(evaluator.eval(clause, env))) {
        return Begin.BEGIN.eval(subform, env, evaluator);
      }
    }
    return SCMConstant.UNSPECIFIED;
  }

  @Override
  public String toString() {
    return "cond";
  }
}
