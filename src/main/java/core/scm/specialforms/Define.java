package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;
import core.writer.Writer;

import java.util.List;

/* Syntax:
 * (define <variable> <expression>)
 * (define (<variable> <formals>) <body>)
 * (define (<variable> . <formal>) <body>)
 */
public enum Define implements ISpecialForm {
  DEFINE;

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    if (expression.size() < 3) {
      throw IllegalSyntaxException.of(toString(), expression);
    }
    Object id = expression.get(1);
    /* Variable definition: (define <id> <value>) */
    if (id instanceof SCMSymbol) {
      if (expression.size() > 3) {
        throw IllegalSyntaxException.of(toString(), expression, "multiple expressions after identifier");
      }
      env.put(id, evaluator.eval(expression.get(2), env));
    } else if (id instanceof SCMCons) {
      /* Procedure definition: (define <id> <proc>) */
      /* Function shorthand definition
       * expression = (define (name a1 a2 ... an [. ar]) f1 f2 ... fn)
       *              |   0   | 1 definition           | 3 body      |
       */
      /* Construct lambda form */
      SCMCons<Object> l = SCMCons.list(Lambda.LAMBDA);

      /* Args */
      SCMCons args = SCMCons.list(((List) expression.get(1)).subList(1, ((List) expression.get(1)).size()));
      for (Object arg : args) {
        if (!(arg instanceof SCMSymbol) && !(SCMCons.isPair(arg))) {
          throw IllegalSyntaxException.of(toString(), expression, String.format("not an identifier: %s", Writer.write(arg)));
        }
      }
      args.setIsList(SCMCons.isList(expression.get(1)));
      l.add(args);

      /* Body */
      l.addAll(expression.subList(2, expression.size()));

      SCMProcedure lambda = Lambda.LAMBDA.eval(l, env, evaluator);

      /* Get procedure's name */
      // TODO (define (((a))) 1)
      // TODO (define ((a n) c) n)
      id = ((SCMCons) id).get(0);
      if (!(id instanceof SCMSymbol)) {
        throw IllegalSyntaxException.of(toString(), expression, String.format("not an identifier for procedure name: %s", Writer.write(id)));
      }
      lambda.setName(id.toString());
      env.put(id, lambda);
    } else {
      throw IllegalSyntaxException.of(toString(), expression);
    }
    return id;
  }

  @Override
  public String toString() {
    return "define";
  }
}
