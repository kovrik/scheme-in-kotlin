package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;
import core.scm.SCMUnspecified;

import java.util.List;

import static core.scm.SCMUnspecified.UNSPECIFIED;

/* Syntax:
 * (define <variable> <expression>)
 * (define (<variable> <formals>) <body>)
 * (define (<variable> . <formal>) <body>)
 */
public enum Define implements ISpecialForm {
  DEFINE;

  @Override
  public SCMUnspecified eval(List<Object> expression, Environment env, Evaluator evaluator) {
    if (expression.size() < 3) {
      throw IllegalSyntaxException.of(toString(), expression);
    }

    Object id = expression.get(1);
    if (id instanceof SCMSymbol) {
      /* Variable definition */
      if (expression.size() > 3) {
        throw IllegalSyntaxException.of(toString(), expression, "multiple expressions after identifier");
      }
      Object body = expression.get(2);
      env.put(id, evaluator.eval(body, env));
    } else if (id instanceof SCMCons) {
      /* Function shorthand definition
       * expression = (define (name a1 a2 ... an [. ar]) f1 f2 ... fn)
       *              |   0   | 1 definition           | 3 body      |
       */
      /* Construct lambda form */
      SCMCons<Object> l = SCMCons.list(Lambda.LAMBDA);

      /* Args */
      SCMCons args = SCMCons.list(((List) expression.get(1)).subList(1, ((List) expression.get(1)).size()));
      args.setIsList(SCMCons.isList(expression.get(1)));
      l.add(args);

      /* Body */
      l.addAll(expression.subList(2, expression.size()));

      SCMProcedure lambda = Lambda.LAMBDA.eval(l, env, evaluator);

      /* Get procedure's name */
      // TODO (define (((a))) 1)
      // TODO (define ((a n) c) n)
      SCMSymbol name = (SCMSymbol)((SCMCons)id).get(0);
      lambda.setName(name.toString());
      env.put(name, lambda);
    } else {
      throw IllegalSyntaxException.of(toString(), expression);
    }
    return UNSPECIFIED;
  }

  @Override
  public String toString() {
    return "define";
  }
}
