package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.*;

import java.util.List;

import static core.scm.SCMUnspecified.UNSPECIFIED;
import static core.scm.specialforms.Lambda.LAMBDA;

/* Syntax:
 * (define <variable> <expression>)
 * (define (<variable> <formals>) <body>)
 * (define (<variable> . <formal>) <body>)
 */
public class Define implements ISpecialForm, ISCMClass {

  public static final Define DEFINE = new Define();

  private final String syntax = "define";
  private final SCMSymbol symbol = new SCMSymbol(this.syntax);

  private Define() {}

  // TODO Check that internal definitions are at the beginning only!
  @Override
  public SCMUnspecified eval(List<Object> expression, IEnvironment env, IEvaluator evaluator) {
    Object id = expression.get(1);
    if (id instanceof SCMSymbol) {
      /* Variable definition */
      if (expression.size() > 3) {
        throw new IllegalSyntaxException("define: bad syntax (multiple expressions after identifier)");
      }
      Object body = expression.get(2);
      env.put(id, evaluator.eval(body, env));
    } else if (id instanceof SCMCons) {
      /* Function shorthand definition
       * expression = (define (name a1 a2 ... an [. ar]) f1 f2 ... fn)
       *              |   0   | 1 definition           | 3 body      |
       */
      /* Get procedure's name */
      SCMSymbol name = (SCMSymbol)((SCMCons)id).pop();
      /* Evaluate lambda */
      expression.set(0, LAMBDA);
      SCMProcedure lambda = LAMBDA.eval(expression, env, evaluator);
      /* Set name */
      lambda.setName(name.getValue());
      env.put(name, lambda);
    } else {
      throw new IllegalSyntaxException("define: bad `define` in form: " + expression);
    }
    return UNSPECIFIED;
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
