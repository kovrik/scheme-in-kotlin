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

  @Override
  public SCMUnspecified eval(List<Object> expression, IEnvironment env, IEvaluator evaluator) {
    if (expression.size() == 1) {
      throw IllegalSyntaxException.of(syntax, expression);
    }
    Object id = expression.get(1);

    // TODO Check if this is an Internal Definition (has non-null outer environment)
    if (env.getOuter() != null) {
      // TODO Check that internal DEFINES are top-only forms!!!
    }

    if (id instanceof SCMSymbol) {
      /* Variable definition */
      if (expression.size() < 3) {
        throw IllegalSyntaxException.of(syntax, expression, "missing expression after identifier");
      }
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
      // TODO Cleanup
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
      /* Set name */
      lambda.setName(name.getValue());

      /* TODO Fix (inline pure proc calls only and at call site only) and optimize
       * See Knuth's Man Or Boy Test */
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
            if (next instanceof SCMSymbol && lambda.getName().equals(((SCMSymbol) next).getValue())) {
              /* Replace symbol with procedure (self-call) */
              listIterator.set(lambda);
            }
          }
        }
      }
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
