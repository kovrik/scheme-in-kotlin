package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.Cons;
import core.scm.Procedure;
import core.scm.Symbol;
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
    if (id instanceof Symbol) {
      if (expression.size() > 3) {
        throw IllegalSyntaxException.of(toString(), expression, "multiple expressions after identifier");
      }
      env.put(id, evaluator.eval(expression.get(2), env));
    } else if (id instanceof Cons) {
      /* Procedure definition: (define <id> <proc>) */
      /* Function shorthand definition
       * expression = (define (name a1 a2 ... an [. ar]) f1 f2 ... fn)
       *              |   0   | 1 definition           | 3 body      |
       */
      /* Construct lambda form */
      Cons<Object> l = Cons.list(Lambda.LAMBDA);
      /* Args */
      Cons args = Cons.list(((List) expression.get(1)).subList(1, ((List) expression.get(1)).size()));
      for (Object arg : args) {
        if (!(arg instanceof Symbol) && !(Cons.isPair(arg))) {
          throw IllegalSyntaxException.of(toString(), expression, String.format("not an identifier: %s", Writer.write(arg)));
        }
      }
      args.setIsList(Cons.isList(expression.get(1)));
      l.add(args);
      /* Body */
      l.addAll(expression.subList(2, expression.size()));
      /* Get procedure's name */
      // TODO (define (((a))) 1)
      // TODO (define ((a n) c) n)
      id = ((Cons) id).get(0);
      if (!(id instanceof Symbol)) {
        throw IllegalSyntaxException.of(toString(), expression, String.format("not an identifier for procedure name: %s", Writer.write(id)));
      }
      Procedure lambda = Lambda.LAMBDA.eval(l, env, evaluator);
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
