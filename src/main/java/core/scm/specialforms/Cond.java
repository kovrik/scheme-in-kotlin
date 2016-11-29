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
public class Cond implements ISpecialForm, ISCMClass {

  public static final Cond COND = new Cond();
  private static final SCMSymbol ELSE = new SCMSymbol("else");

  private final String syntax = "cond";
  private final SCMSymbol symbol = new SCMSymbol(this.syntax);

  private Cond() {}

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
          return new TailCall(subform.get(subform.size() - 1), env);
        }
        throw IllegalSyntaxException.of(syntax, expression, "else must be the last clause in subform");
      }
      if (SCMBoolean.valueOf(evaluator.eval(clause, env))) {
        for (int s = 1; s < subform.size() - 1; s++) {
          evaluator.eval(subform.get(s), env);
        }
        return new TailCall(subform.get(subform.size() - 1), env);
      }
    }
    return SCMUnspecified.UNSPECIFIED;
  }

  public SCMSymbol symbol() {
    return symbol;
  }

  @Override
  public String toString() {
    return syntax;
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.SPECIALFORM;
  }
}
