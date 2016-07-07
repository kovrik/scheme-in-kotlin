package core.evaluator;

import core.environment.Environment;
import core.environment.IEnvironment;
import core.procedures.IFn;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;
import core.scm.specialforms.SCMSpecialForm;

import java.util.Arrays;
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
    } else if (sexp instanceof SCMCons) {
      return evlis(sexp, env);
    }
    throw new IllegalArgumentException("Evaluation error: " + sexp);
  }

  /**
   * Gets a procedure `fn`, parameters `args`, environment `env`,
   * invokes the procedure and returns a result.
   */
  public Object apply(Object fn, Object[] args, IEnvironment env) {

    SCMProcedure procedure = (SCMProcedure) fn;
    List<SCMSymbol> params = procedure.getParams();
    if (!procedure.isVariableArity()) {
      if (args.length != params.size()) {
        procedure.throwArity(args.length);
      }
    }
    Map<Object, Object> values = new HashMap<Object, Object>(params.size());
    if (!procedure.isVariableArity()) {
      for (int i = 0; i < params.size(); i++) {
        values.put(params.get(i), args[i]);
      }
    } else {
      /* Variadic arity procedure */
      /* Put mandatory params first */
      for (int i = 0; i < params.size() - 1; i++) {
        values.put(params.get(i), args[i]);
      }
      /* Then rest */
      // TODO Cleanup and optimize
      List<Object> varargs = SCMCons.list();
      varargs.addAll(Arrays.asList(Arrays.copyOfRange(args, params.size() - 1, args.length)));
      values.put(params.get(params.size() - 1), varargs);
    }
    return procedure.apply(this, new Environment(values, env));
  }

  /**
   * Evaluate a list
   */
  public Object evlis(Object sexp, IEnvironment env) {

    SCMCons list = (SCMCons)sexp;
    if (list.isEmpty()) {
      throw new IllegalArgumentException("Unexpected syntax in form " + list);
    }
    Object op = list.getFirst();

    /* Special Form */
    if (op instanceof SCMSpecialForm) {
      return ((SCMSpecialForm)op).eval(list, env, this);
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
    if (fn instanceof SCMProcedure) {
      IEnvironment closure = ((SCMProcedure) fn).getClosure();
      if (closure == null) {
        closure = env;
      }
      return apply(fn, args, closure);
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
