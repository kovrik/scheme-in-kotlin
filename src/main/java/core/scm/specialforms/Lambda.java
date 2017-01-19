package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;

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
  public SCMProcedure eval(List<Object> expression, Environment env, Evaluator evaluator) {
    if (expression.size() < 3) {
      throw IllegalSyntaxException.of(toString(), expression);
    }

    List<SCMSymbol> params;
    boolean variadic = false;
    /* Check if args is a List or not */
    Object args = expression.get(1);
    if (args instanceof List) {
      /* Check args for duplicates */
      if (!((List) args).isEmpty()) {
        HashSet<Object> temp = new HashSet<>(((List) args).size());
        for (Object o : ((List) args)) {
          if (temp.contains(o)) {
            throw IllegalSyntaxException.of(toString(), expression, String.format("duplicate argument identifier `%s`", o));
          }
          temp.add(o);
        }
      }
      /* (lambda (arg-id ...+) body ...+) OR
       * (lambda (arg-id ...+ . rest-id) body ...+) */
      if (SCMCons.isList(args)) {
        /* args is a proper list, hence non-variadic lambda */
        params = (List<SCMSymbol>)args;
      } else {
        /* args is an improper list, hence variadic lambda */
        params = SCMCons.flatten((List<SCMSymbol>)args);
        variadic = true;
      }
    } else {
      /* Variadic arity */
      /* (lambda rest-id body ...+) */
      if (!(args instanceof SCMSymbol)) {
        throw new IllegalSyntaxException(String.format("lambda: bad argument sequence (%s) in form: %s",  args, expression));
      }
      params = SCMCons.list((SCMSymbol)args);
      variadic = true;
    }
    Object body;
    if (expression.size() == 3) {
      body = expression.get(2);
    } else {
      /* Add implicit `begin` */
      body = SCMCons.list(Begin.BEGIN);
      ((List)body).addAll(expression.subList(2, expression.size()));
    }
    return new SCMProcedure("", params, body, env, variadic);
  }

  @Override
  public String toString() {
    return "lambda";
  }
}
