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
import core.scm.specialforms.tco.TailCall;
import core.writer.Writer;

import java.util.ArrayList;
import java.util.List;

public class Evaluator implements IEvaluator {

  @Override
  public Object eval(Object sexp, IEnvironment env) {
    /* TCO: Trampoline */
    Object result = evalp(sexp, env);
    while (result instanceof TailCall) {
      result = evalp(((TailCall)result).getExpr(), ((TailCall) result).getContext());
    }
    return result;
  }

  // TODO Rename
  private Object evalp(Object sexp, IEnvironment env) {
    if (sexp instanceof SCMSymbol) {
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
    List<Object> list = (List<Object>)sexp;
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
    if (!(fn instanceof IFn)) {
      /* Can apply IFn only */
      throw new IllegalArgumentException("Wrong type to apply: " + Writer.write(fn));
    }

    /* Evaluate arguments first (because applicative order) */
    List<Object> args = new ArrayList<Object>(list.size() - 1);
    for (int i = 1; i < list.size(); i++) {
      args.add(eval(list.get(i), env));
    }

    // TODO Convert to CPS

    /* Scheme procedure (lambda) */
    if (fn instanceof SCMProcedure) {
      return apply((SCMProcedure)fn, args);
    }

    /* IFn (built-in Java function) */
    // TODO Introduce 0,1,2..N-arity invoke() to improve performance?
    Object result = ((IFn)fn).invoke(args.toArray());

    /* Handle Promise forced to evaluation by Force procedure */
    if ((result instanceof SCMPromise) && ((SCMPromise)result).getState() == SCMPromise.State.FORCED) {
      result = evalForcedPromise((SCMPromise)result, env);
    }
    return result;
  }

  /**
   * Apply SCMProcedure
   */
  private Object apply(SCMProcedure fn, List<Object> args) {
    List<SCMSymbol> params = fn.getArgs();

    /* Variadic procedures keep last param to store list of rest (optional) params */
    int mandatoryParamsSize = fn.isVariadic() ? params.size() - 1 : params.size();

    /* Check arity (mandatory params):
     * - non-variadic function should get expected number of mandatory arguments
     * - variadic function should get expected number of mandatory arguments or more (but not less) */
    if ((fn.isVariadic()  && (args.size() <  mandatoryParamsSize)) ||
        (!fn.isVariadic() && (args.size() != mandatoryParamsSize))) {

      throw new ArityException(args.size(), params.size(), fn.getName());
    }
    // TODO Is it always necessary to create new local Environment every time?
    /* Evaluate mandatory params and put values into new local environment */
    IEnvironment localEnvironment = new Environment(fn.getLocalEnvironment());
    for (int i = 0; i < mandatoryParamsSize; i++) {
      localEnvironment.put(params.get(i), args.get(i));
    }

    /* If it is a variadic function, then evaluate rest params */
    if (fn.isVariadic()) {
      /* Optional params: pass them as a list bound to the last param.
       * Everything AFTER mandatory params goes to that list. */
      List<Object> varargs = SCMCons.list(args.subList(mandatoryParamsSize, args.size()));
      localEnvironment.put(params.get(mandatoryParamsSize), varargs);
    }
    /* Closure is ready to be evaluated */
    return evlis(fn.getBody(), localEnvironment);
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
}
