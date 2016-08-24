package core.scm.specialforms.tco;

import core.environment.Environment;
import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.*;
import core.scm.specialforms.ISpecialForm;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static core.scm.SCMUnspecified.UNSPECIFIED;

/* Syntax:
 * (do <bindings> <clause> <body>)
 *
 * <bindings>: ((<variable 1> <init 1> <step 1>) ...),
 * <clause>:   (<test> <expression> ...),
 **/
public class Do implements ISpecialForm, ISCMClass {

  public static final Do DO = new Do();

  private final String syntax = "do";
  private final SCMSymbol symbol = new SCMSymbol(this.syntax);

  private Do() {}

  @Override
  public Object eval(List<Object> expression, IEnvironment env, IEvaluator evaluator) {
    if (expression.size() < 3) {
      throw new IllegalSyntaxException("do: bad syntax");
    }

    Object bs = expression.get(1);
    if (!(bs instanceof List)) {
      throw new IllegalSyntaxException("do: bad syntax");
    }
    IEnvironment tempEnv = new Environment(env);
    List bindings = (List) bs;
    List<SCMCons> steps = SCMCons.list();
    /* Init bindings */
    for (Object binding : bindings) {
      if (!(binding instanceof List)) {
        throw new IllegalSyntaxException("do: bad syntax");
      }
      /* Check that init value exists */
      if (((List)binding).size() < 2) {
        throw new IllegalSyntaxException("do: bad syntax");
      }
      Object variable = ((List) binding).get(0);
      Object init = ((List) binding).get(1);
      Object step;
      if (((List) binding).size() == 3) {
        step = ((List) binding).get(2);
        /* Put pair of Var and Step */
        steps.add(SCMCons.cons(variable, step));
      }
      /* Check that we have no duplicates among variables */
      if (!tempEnv.containsKey(variable)) {
        tempEnv.put(variable, evaluator.eval(init, tempEnv));
      } else {
        throw new IllegalSyntaxException("let: duplicate identifier: " + variable);
      }
    }

    Object cl = expression.get(2);
    if (!(cl instanceof List)) {
      throw new IllegalSyntaxException("do: bad syntax");
    }
    List clause = (List)cl;
    if (clause.isEmpty()) {
      throw new IllegalSyntaxException("do: bad syntax");
    }

    Object test = clause.get(0);
    List body = expression.subList(3, expression.size());
    /* While test evaluates to #f */
    while (!SCMBoolean.valueOf(evaluator.eval(test, tempEnv))) {
      /* Evaluate command expressions */
      for (Object e : body) {
        /* Evaluate each expression */
        evaluator.eval(e, tempEnv);
      }
      /* Evaluate steps */
      Map<Object, Object> freshLocations = new HashMap<>(steps.size());
      for (SCMCons step : steps) {
        Object variable = step.car();
        Object s = step.cdr();
        freshLocations.put(variable, evaluator.eval(s, tempEnv));
      }
      /* Now store results */
      for (Map.Entry<Object, Object> entry : freshLocations.entrySet()) {
        tempEnv.put(entry.getKey(), entry.getValue());
      }
    }
    /* Test evaluated to #f */
    List expressions = clause.subList(1, clause.size());
    Object value = UNSPECIFIED;
    for (int i = 0; i < expressions.size() - 1; i++) {
      /* Evaluate each expression */
      value = evaluator.eval(expressions.get(i), tempEnv);
    }
    /* Return Tail Call of last expression or UNSPECIFIED */
    // TODO Check UNSPECIFIED value?
    return new TailCall(expressions.get(expressions.size() - 1), tempEnv);
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
