package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;
import core.scm.SCMUnspecified;

import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;

import static core.scm.SCMUnspecified.UNSPECIFIED;

/* Syntax:
 * (define <variable> <expression>)
 * (define (<variable> <formals>) <body>)
 * (define (<variable> . <formal>) <body>)
 */
public enum Define implements ISpecialForm {
  DEFINE;

  private static final String syntax = "define";

  @Override
  public SCMUnspecified eval(List<Object> expression, Environment env, Evaluator evaluator) {
    if (expression.size() < 3) {
      throw IllegalSyntaxException.of(syntax, expression);
    }
    evaluator.checkDefinitionContext(expression, env);

    Object id = expression.get(1);
    if (id instanceof SCMSymbol) {
      /* Variable definition */
      if (expression.size() > 3) {
        throw IllegalSyntaxException.of(syntax, expression, "multiple expressions after identifier");
      }
      Object body = expression.get(2);
      env.put(id, evaluator.eval(body, env));
    } else if (id instanceof SCMCons) {
      /* Function shorthand definition
       * expression = (define (name a1 a2 ... an [. ar]) f1 f2 ... fn)
       *              |   0   | 1 definition           | 3 body      |
       */
      // TODO (define (((a))) 1) should work
      /* Get procedure's name */
      SCMSymbol name = (SCMSymbol)((SCMCons)id).get(0);
      /* Evaluate lambda */
      /* Args */
      SCMCons args = SCMCons.list(((List) expression.get(1)).subList(1, ((List) expression.get(1)).size()));
      args.setIsList(SCMCons.isList(expression.get(1)));

      SCMCons<Object> l = SCMCons.list(Lambda.LAMBDA);
      l.add(args);
      /* Body */
      l.addAll(expression.subList(2, expression.size()));

      SCMProcedure lambda = Lambda.LAMBDA.eval(l, env, evaluator);
      lambda.setName(name.toString());

      if (lambda.isPure()) {
        replaceSelfCalls(lambda);
      }
      env.put(name, lambda);
    } else {
      throw IllegalSyntaxException.of(syntax, expression);
    }
    return UNSPECIFIED;
  }

  /* TODO Fixme:
   * - not sure if works in all cases
   * - generify
   * - inline pure functions only?
   * - inline at call sites only?
   *
   * See Knuth's Man Or Boy Test
   */
  private static void replaceSelfCalls(SCMProcedure lambda) {
    LinkedList<List> queue = new LinkedList<>();
    /* Queue will hold body and all nested lists (if any) */
    queue.add(lambda.getBody());
    /* `first` flag is used to track if current element is the first element of a List,
     * hence assuming it is a proc call site, therefore we can inline it.
     * Otherwise, it is just a symbol, do not inline it.
     * See: Knuth's Man or Boy test */
    while (!queue.isEmpty()) {
      List list = queue.remove();
      /* Using ListIterator because it allows element modification */
      boolean first = true;
      ListIterator listIterator = list.listIterator();
      while (listIterator.hasNext()) {
        Object next = listIterator.next();
        if (next instanceof List) {
          /* Add nested list to the queue */
          queue.add((List) next);
          first = true;
        } else {
          if (first) {
            first = false;
            if (next instanceof SCMSymbol && lambda.getName().equals(next.toString())) {
              /* Replace symbol with procedure (self-call) */
              listIterator.set(lambda);
            }
          }
        }
      }
    }
  }

  @Override
  public String toString() {
    return syntax;
  }
}
