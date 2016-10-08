package core.scm.specialforms;

import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.*;

import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;

import static core.scm.SCMUnspecified.UNSPECIFIED;

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
      // TODO Cleanup
      /* Get procedure's name */
      SCMSymbol name = (SCMSymbol)((SCMCons)id).get(0);
      /* Evaluate lambda */
      SCMCons<Object> l = SCMCons.list(Lambda.LAMBDA);
      /* Args */
      SCMCons args = SCMCons.list(((List) expression.get(1)).subList(1, ((List) expression.get(1)).size()));
      args.setIsList(SCMCons.isList(expression.get(1)));
      l.add((Object)args);
      /* Body */
      l.addAll(expression.subList(2, expression.size()));

      SCMProcedure lambda = Lambda.LAMBDA.eval(l, env, evaluator);
      /* Set name */
      lambda.setName(name.getValue());
      // TODO Optimize
      replaceSelfCalls(lambda);
      env.put(name, lambda);
    } else {
      throw new IllegalSyntaxException("define: bad `define` in form: " + expression);
    }
    return UNSPECIFIED;
  }

  /* TODO Fix (not always inline, only if lambda is pure!) and generify */
  private static void replaceSelfCalls(SCMProcedure lambda) {
//    LinkedList<List> queue = new LinkedList<List>();
//    /* Queue will hold body and all nested lists (if any) */
//    queue.add(lambda.getBody());
//    while (!queue.isEmpty()) {
//      List list = queue.remove();
//      /* Using ListIterator because it allows element modification */
//      ListIterator listIterator = list.listIterator();
//      while (listIterator.hasNext()) {
//        Object next = listIterator.next();
//        if (next instanceof List) {
//          /* Add nested list to the queue */
//          queue.add((List) next);
//        } else {
//          if (next instanceof SCMSymbol && lambda.getName().equals(((SCMSymbol) next).getValue())) {
//            /* Replace symbol with procedure (self-call) */
//            listIterator.set(lambda);
//          }
//        }
//      }
//    }
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
