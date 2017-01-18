package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.IllegalSyntaxException;
import core.scm.*;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/* Syntax:
 * (do <bindings> <clause> <body>)
 *
 * <bindings>: ((<variable 1> <init 1> <step 1>) ...),
 * <clause>:   (<test> <expression> ...),
 **/
public enum Do implements ISpecialForm {
  DO;

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {
    if (expression.size() < 3) {
      throw IllegalSyntaxException.of(toString(), expression);
    }

    Object bs = expression.get(1);
    if (!(bs instanceof List)) {
      throw IllegalSyntaxException.of(toString(), expression);
    }
    Environment tempEnv = new Environment(env);
    List bindings = (List) bs;
    List<SCMCons> steps = SCMCons.list();
    // TODO Replace with call to LET
    /* Init bindings */
    for (Object binding : bindings) {
      if (!(binding instanceof List)) {
        throw IllegalSyntaxException.of(toString(), expression);
      }
      /* Check that init value exists */
      if (((List)binding).size() < 2) {
        throw IllegalSyntaxException.of(toString(), expression);
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
        throw IllegalSyntaxException.of(Let.LET.toString(), expression, "duplicate identifier");
      }
    }

    Object cl = expression.get(2);
    if (!(cl instanceof List)) {
      throw IllegalSyntaxException.of(toString(), expression);
    }
    List<Object> clause = (List<Object>)cl;
    if (clause.isEmpty()) {
      throw IllegalSyntaxException.of(toString(), expression);
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
    return Begin.BEGIN.eval(clause.subList(1, clause.size()), tempEnv, evaluator);
  }

  @Override
  public String toString() {
    return "do";
  }
}
