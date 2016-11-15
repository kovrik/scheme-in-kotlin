package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.IllegalSyntaxException;
import core.procedures.AFn;
import core.scm.*;

import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;

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
    // TODO Is implicit BEGIN enough to have TCO?
    SCMCons<Object> body = SCMCons.list(Begin.BEGIN);
    body.addAll(expression.subList(2, expression.size()));

    /* Optimization: replace some symbols with their values */
    // FIXME Should inline symbols at call sites only! See Knuth's Man Or Boy Test
//    inline(body, env);

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

  // TODO Check if it works properly. See Define.replaceSelfCalls()
  private static void inline(List<Object> body, IEnvironment env) {
    LinkedList<List> queue = new LinkedList<List>();
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
