package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.Cons;
import core.scm.Procedure;
import core.scm.Symbol;

import java.util.HashSet;
import java.util.List;

/* Syntax:
 * (lambda <formals> <body>)
 *
 * <formals>:
 * (<variable1> ...)
 * <variable>
 * (<variable1> ... <variablen> . <variablen+1>)
 */
public enum Lambda implements ISpecialForm {
  LAMBDA;

  @Override
  public Procedure eval(List<Object> expression, Environment env, Evaluator evaluator) {
    if (expression.size() < 3) {
      throw IllegalSyntaxException.of(toString(), expression);
    }

    List<Symbol> params;
    boolean variadic = false;
    /* Check if args is a List or not */
    Object args = expression.get(1);
    if (args instanceof List) {
      /* Check args for duplicates */
      if (!((List) args).isEmpty()) {
        HashSet<Object> temp = new HashSet<>(((List) args).size());
        for (Object o : ((List) args)) {
          if (!(o instanceof Symbol) && !(Cons.isPair(o))) {
            throw IllegalSyntaxException.of(toString(), expression, String.format("not an identifier: %s", o));
          }
          if (temp.contains(o)) {
            throw IllegalSyntaxException.of(toString(), expression, String.format("duplicate argument name: %s", o));
          }
          temp.add(o);
        }
      }
      /* (lambda (arg-id ...+) body ...+) OR
       * (lambda (arg-id ...+ . rest-id) body ...+) */
      if (Cons.isList(args)) {
        /* args is a proper list, hence non-variadic lambda */
        params = (List<Symbol>)args;
      } else {
        /* args is an improper list, hence variadic lambda */
        params = Cons.flatten((List<Symbol>)args);
        variadic = true;
      }
    } else {
      /* Variadic arity */
      /* (lambda rest-id body ...+) */
      if (!(args instanceof Symbol)) {
        throw new IllegalSyntaxException(String.format("lambda: bad argument sequence (%s) in form: %s",  args, expression));
      }
      params = Cons.list((Symbol)args);
      variadic = true;
    }
    Object body;
    if (expression.size() == 3) {
      body = expression.get(2);
    } else {
      /* Add implicit `begin` */
      body = Cons.list(Begin.BEGIN);
      ((List)body).addAll(expression.subList(2, expression.size()));
    }
    return new Procedure("", params, body, env, variadic);
  }

  @Override
  public String toString() {
    return "lambda";
  }
}
