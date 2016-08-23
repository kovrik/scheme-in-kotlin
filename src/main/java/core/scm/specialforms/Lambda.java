package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.*;

import java.util.List;

import static core.scm.specialforms.tco.Begin.BEGIN;

//import static core.scm.specialforms.Begin.BEGIN;

/* Syntax:
 * (lambda <formals> <body>)
 *
 * <formals>:
 * (<variable1> ...)
 * <variable>
 * (<variable1> ... <variablen> . <variablen+1>)
 */
public class Lambda implements ISpecialForm, ISCMClass {

  public static final Lambda LAMBDA = new Lambda();

  private final String syntax = "lambda";
  private final SCMSymbol symbol = new SCMSymbol(this.syntax);

  private Lambda() {}

  @Override
  public SCMProcedure eval(List<Object> expression, IEnvironment env, IEvaluator evaluator) {
    if (expression.size() < 3) {
      throw new IllegalSyntaxException("lambda: bad lambda in form: " + expression);
    }
    /* Add implicit `begin` */
    SCMCons<Object> body = SCMCons.list(BEGIN);
    body.addAll(expression.subList(2, expression.size()));
    /* Check if args is a List or not */
    Object args = expression.get(1);
    if (args instanceof List) {
      /* (lambda (arg-id ...+) body ...+) OR
       * (lambda (arg-id ...+ . rest-id) body ...+) */
      if (SCMCons.isList(args)) {
        /* args is a proper list, hence non-variadic lambda */
        return new SCMProcedure("", (List<SCMSymbol>)args, body, env);
      } else {
        /* args is an improper list, hence variadic lambda */
        List<SCMSymbol> params = SCMCons.flatten((List<SCMSymbol>)args);
        return new SCMProcedure("", params, body, env, true);
      }
    } else {
      /* Variadic arity */
      /* (lambda rest-id body ...+) */
      return new SCMProcedure("", SCMCons.list((SCMSymbol)args), body, env, true);
    }
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
