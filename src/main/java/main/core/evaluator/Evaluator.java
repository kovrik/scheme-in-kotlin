package main.core.evaluator;

import main.core.ast.SCMList;
import main.core.ast.SCMSymbol;
import main.core.environment.Environment;
import main.core.environment.IEnvironment;
import main.core.exceptions.ArityException;
import main.core.procedures.IFn;
import main.core.procedures.Procedure;
import main.core.specialforms.SpecialForm;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;

public class Evaluator implements IEvaluator {

  public Object eval(Object sexp, IEnvironment env) {

    if (sexp instanceof SCMSymbol) {
      /* Symbol */
      if (((SCMSymbol) sexp).getValue().startsWith(",")) {
        /* Meta */
        return evmeta(((SCMSymbol)sexp).getValue());
      }
      return env.find(sexp);
    } else if (!(sexp instanceof List)) {
      return sexp;
    } else if (sexp instanceof SCMList) {
      return evlis(sexp, env);
    }
    throw new IllegalArgumentException("Evaluation error: " + sexp);
  }

  /**
   * Gets a procedure `fn`, parameters `args`, environment `env`,
   * invokes the procedure and returns a result.
   */
  public Object apply(Object fn, Object[] args, IEnvironment env) {

    Procedure procedure = (Procedure) fn;
    List<Object> params = procedure.getParams();
    if (args.length != params.size()) {
      throw new ArityException(args.length, procedure.getClass().getSimpleName());
    }
    Map<Object, Object> values = new HashMap<Object, Object>(params.size());
    for (int i = 0; i < params.size(); i++) {
      values.put(params.get(i), args[i]);
    }
//    return eval(procedure.getBody(), new Environment(values, env));
    return procedure.apply(this, new Environment(values, env));
  }

  /**
   * Evaluate a list
   */
  public Object evlis(Object sexp, IEnvironment env) {

    SCMList<Object> list = (SCMList<Object>)sexp;
    if (list.isEmpty()) {
      throw new IllegalArgumentException("Unexpected syntax in form " + list);
    }
    Object op = list.getFirst();

    /* Special Form */
    if (op instanceof SpecialForm) {
      return ((SpecialForm)op).eval(list, env, this);
    }
    /* Function */
    Object fn = eval(op, env);
    if (fn == null) {
      throw new UnsupportedOperationException("Unbound variable: " + op);
    }
    /* Evaluate arguments */
    Object[] args = new Object[list.size() - 1];
    for (int i = 1; i < list.size(); i++) {
      args[i - 1] = eval(list.get(i), env);
    }
    /* Procedure */
    if (!(fn instanceof IFn)) {
      throw new IllegalArgumentException("Wrong type to apply: " + fn);
    }
    // FIXME Make generic as IFn?
    if (fn instanceof Procedure) {
      return apply(fn, args, env);
    }
    try {
      return ((IFn)fn).invoke(args);
    } catch (ExecutionException e) {
      e.printStackTrace();
    } catch (InterruptedException e) {
      e.printStackTrace();
    }
    throw new IllegalArgumentException("Evaluation error: " + sexp);
  }

  /**
   * Evaluate meta
   */
  public Object evmeta(String sexp) {
    if (",q".equals(sexp)) {
      System.out.println("Bye!");
      System.exit(0);
    }
    throw new IllegalArgumentException("Evaluation error: " + sexp);
  }
}
