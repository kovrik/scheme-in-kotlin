package core.evaluator;

import core.environment.Environment;
import core.environment.IEnvironment;
import core.exceptions.ArityException;
import core.exceptions.IllegalSyntaxException;
import core.procedures.IFn;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMPromise;
import core.scm.SCMSymbol;
import core.scm.specialforms.ISpecialForm;
import core.writer.Writer;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Evaluator implements IEvaluator {

  @Override
  public Object eval(Object sexp, IEnvironment env) {
    if (sexp instanceof SCMSymbol) {
      if (((SCMSymbol)sexp).getValue().startsWith(",")) {
        return evmeta(((SCMSymbol) sexp).getValue());
      }
      /* Check if it is a Special Form */
      Object o = env.find(sexp);
      if (o instanceof ISpecialForm) {
        throw new IllegalSyntaxException("Unexpected syntax in form: " + o);
      }
      return o;
    } else if (!(sexp instanceof List)) {
      return sexp;
    } else {
      return evlis(sexp, env);
    }
  }

  /**
   * Evaluate a list
   */
  private Object evlis(Object sexp, IEnvironment env) {
    List list = (List)sexp;
    if (list.isEmpty()) {
      throw new IllegalSyntaxException("Unexpected syntax in form " + list);
    }
    /* Check if op is a Special Form.
     * This is used for implicit Special Forms
     * used in other Special Forms (like BEGIN in LAMBDA).
     * We should be able to eval such forms even if user
     * has redefined implicit Special Form */
    Object op = list.get(0);
    if (op instanceof ISpecialForm) {
      return ((ISpecialForm)op).eval(list, env, this);
    }
    /* Check if Symbol refers to a Special Form */
    if (op instanceof SCMSymbol) {
      /* Get it from the environment: let user redefine special forms */
      Object specialForm = env.find(op);
      if (specialForm instanceof ISpecialForm) {
        return ((ISpecialForm)specialForm).eval(list, env, this);
      }
    }

    /* Must be a procedure */
    Object fn = eval(op, env);
    if (!(fn instanceof IFn) || (fn instanceof SCMPromise)) {
      /* Can apply IFn only */
      throw new IllegalArgumentException("Wrong type to apply: " + Writer.write(fn));
    }

    /* Evaluate arguments (because applicative order) */
    List<Object> args = new ArrayList<Object>(list.size() - 1);
    for (int i = 1; i < list.size(); i++) {
      args.add(eval(list.get(i), env));
    }

    /* Scheme procedure */
    if (fn instanceof SCMProcedure) {
      IEnvironment closure = ((SCMProcedure)fn).getClosure();
      closure = (closure == null) ? env : closure;
      return apply((SCMProcedure)fn, args, closure);
    }

    /* IFn */
    Object result = ((IFn)fn).invoke(args.toArray());
    if ((result instanceof SCMPromise) && ((SCMPromise)result).getState() == SCMPromise.State.FORCED) {
      /* Handle Promise forced to evaluation by Force procedure */
      result = evalForcedPromise((SCMPromise)result, env);
    }
    return result;
  }

  private Object apply(SCMProcedure fn, List<Object> args, IEnvironment env) {
    List<SCMSymbol> params = fn.getParams();
    Map<Object, Object> values = new HashMap<Object, Object>(params.size());
    if (!fn.isVariableArity()) {
      /* Check arity */
      if (!fn.isVariableArity() && (args.size() != params.size())) {
        throw new ArityException(args.size(), params.size(), fn.getName());
      }
      for (int i = 0; i < params.size(); i++) {
        values.put(params.get(i), args.get(i));
      }
    } else {
      /* Variadic arity procedure */
      /* Check arity */
      if (args.size() < params.size() - 1) {
        throw new ArityException(args.size(), fn.getName());
      }
      /* Put mandatory params first */
      for (int i = 0; i < params.size() - 1; i++) {
        values.put(params.get(i), args.get(i));
      }
      /* Then rest */
      List<Object> varargs = SCMCons.list(args.subList(params.size() - 1, args.size()));
      values.put(params.get(params.size() - 1), varargs);
    }
    return evlis(fn.getBody(), new Environment(values, env));
  }

  /**
   * Evaluate forced Promise
   */
  private Object evalForcedPromise(SCMPromise promise, IEnvironment env) {
    /* Evaluate the body */
    Object result;
    try {
      result = eval(promise.getBody(), env);
      /* Mark Promise as FULFILLED */
      promise.setState(SCMPromise.State.FULFILLED);
      /* Memoize the result */
      promise.setResult(result);
    } catch (Exception e) {
      result = e;
      /* Mark Promise as REJECTED */
      promise.setState(SCMPromise.State.REJECTED);
      /* Memoize the result */
      promise.setResult(result);
      throw e;
    }
    return result;
  }

  /**
   * Evaluate meta
   */
  private Object evmeta(String sexp) {
    if (",q".equals(sexp)) {
      System.out.println("Bye!");
      System.exit(0);
    }
    throw new IllegalArgumentException("Evaluation error: " + sexp);
  }
}
