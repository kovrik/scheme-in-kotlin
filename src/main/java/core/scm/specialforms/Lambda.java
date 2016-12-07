package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.IllegalSyntaxException;
import core.procedures.AFn;
import core.scm.*;

import java.util.*;

/* Syntax:
 * (lambda <formals> <body>)
 *
 * <formals>:
 * (<variable1> ...)
 * <variable>
 * (<variable1> ... <variablen> . <variablen+1>)
 */
public enum Lambda implements ISpecialForm, ISCMClass {
  LAMBDA;

  private static final String syntax = "lambda";

  @Override
  public SCMProcedure eval(List<Object> expression, IEnvironment env, IEvaluator evaluator) {
    if (expression.size() < 3) {
      throw IllegalSyntaxException.of(syntax, expression);
    }
    /* Add implicit `begin` */
    // TODO Is implicit BEGIN enough to have TCO?
    SCMCons<Object> body = SCMCons.list(Begin.BEGIN);
    body.addAll(expression.subList(2, expression.size()));

    /* Optimization: replace some symbols with their values */
    // FIXME Should inline symbols at call sites only! See Knuth's Man Or Boy Test
//    inline(body, env);

    /* Check if args is a List or not */
    Object args = expression.get(1);
    if (args instanceof List) {
      /* Check args for duplicates */
      if (!((List) args).isEmpty()) {
        Map<Object, Object> temp = new HashMap<>(((List) args).size());
        for (Object o : ((List) args)) {
          if (temp.containsKey(o)) {
            throw IllegalSyntaxException.of(syntax, expression, String.format("duplicate argument identifier `%s`", o));
          }
          temp.put(o, o);
        }
      }
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

  @Override
  public String toString() {
    return syntax;
  }

  // TODO Check if it works properly. See Define.replaceSelfCalls()
  private static void inline(List<Object> body, IEnvironment env) {
    LinkedList<List> queue = new LinkedList<>();
    /* Queue will hold body and all nested lists (if any) */
    queue.add(body);
    while (!queue.isEmpty()) {
      List list = queue.remove();
      /* Using ListIterator because it allows element modification */
      ListIterator listIterator = list.listIterator();
      while (listIterator.hasNext()) {
        Object next = listIterator.next();
        if (next instanceof List) {
          /* Add nested list to the queue */
          queue.add((List) next);
        } else {
          /* Not a list, but an SCMSymbol, candidate for inlining */
          if (next instanceof SCMSymbol) {
            Object o = env.findOrNull(next);
            if ((o instanceof ISpecialForm) ||
                ((o instanceof AFn) && !(o instanceof SCMProcedure))) {

              /* Inline Special Forms and AFns to avoid further lookups */
              listIterator.set(o);
            }
          }
        }
      }
    }
  }
}
