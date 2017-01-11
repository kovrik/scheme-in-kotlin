package core.evaluator;

import core.environment.Environment;
import core.environment.IEnvironment;
import core.exceptions.ArityException;
import core.exceptions.IllegalSyntaxException;
import core.exceptions.ReentrantContinuationException;
import core.procedures.AFn;
import core.procedures.IFn;
import core.procedures.continuations.CallCC;
import core.procedures.continuations.CalledContinuation;
import core.procedures.continuations.Continuation;
import core.procedures.continuations.DynamicWind;
import core.scm.*;
import core.scm.specialforms.ISpecialForm;
import core.writer.Writer;

import java.util.ArrayList;
import java.util.List;

public class Evaluator implements IEvaluator {

  @Override
  public Object eval(Object sexp, IEnvironment env) {
    /* TCO: This is our Trampoline */
    Object result;
    try {
      result = evalIter(sexp, env);
      while (result instanceof SCMTailCall) {
        IEnvironment context = ((SCMTailCall) result).getContext();
        if (context == null) {
          context = env;
        }
        result = evalIter(((SCMTailCall) result).getExpr(), context);
      }
    } catch (CalledContinuation cc) {
      if (!cc.getContinuation().isValid()) {
        /* We have one-shot continuations only, not full continuations.
         * It means that we can't use the same continuation multiple times. */
        throw new ReentrantContinuationException();
      }
      /* Continuation is still valid, rethrow it further (should be caught by callcc)  */
      throw cc;
    }
    // TODO Downcast if possible?
    return result;
  }

  /**
   * One iteration of evaluation.
   * Returns the end result or TailCall object.
   * If TailCall object is returned, then eval() method (trampoline) continues evaluation.
   */
  private Object evalIter(Object sexp, IEnvironment env) {
    if (sexp instanceof SCMSymbol) {
      /* Check if it is a Special Form */
      Object o = env.find(sexp);
      if (o instanceof ISpecialForm) {
        throw IllegalSyntaxException.of(o.toString(), sexp);
      }
      return o;
    } else if (sexp instanceof List) {
      return evlis((List<Object>)sexp, env);
    } else {
      return sexp;
    }
  }

  /**
   * Evaluate a list
   */
  private Object evlis(List<Object> sexp, IEnvironment env) {
    if (sexp.isEmpty()) {
      throw IllegalSyntaxException.of("eval", sexp, "illegal empty application");
    }
    Object op = sexp.get(0);

    /* Lookup symbol */
    if (op instanceof SCMSymbol) {
      op = env.find(op);
    }

    /* If it is a Special Form, then evaluate it */
    if (op instanceof ISpecialForm) {
      // TODO Check if we can actually do this!
      /* Inline Special Form */
      sexp.set(0, op);
      return ((ISpecialForm)op).eval(sexp, env, this);
    }

    /* If it is not AFn, then try to evaluate it (assuming it is a Lambda) */
    if (!(op instanceof AFn)) {
      op = eval(op, env);
      /* If result is not a function, then raise an error */
      if (!(op instanceof AFn)) {
        throw new IllegalArgumentException("Wrong type to apply: " + Writer.write(op));
      }
    }
    AFn fn = (AFn)op;

    // TODO Check if we can actually do this!
    /* Inline pure fns tp avoid further lookups */
    if (fn.isPure())  {
      sexp.set(0, fn);
    }

    /* Scheme has applicative order, so evaluate all arguments first */
    List<Object> args = new ArrayList<>(sexp.size() - 1);
    for (int i = 1; i < sexp.size(); i++) {
      args.add(eval(sexp.get(i), env));
    }
    /* Check args */
    fn.checkArgs(args);

    /* call-with-current-continuation */
    if (fn instanceof CallCC) {
      return callcc((IFn) args.get(0), env);
    }
    /* dynamic-wind */
    if (fn instanceof DynamicWind) {
      return dynamicWind((IFn)args.get(0), (IFn)args.get(1), (IFn)args.get(2), env);
    }
    /* Scheme procedure (lambda) */
    if (fn instanceof SCMProcedure) {
      return apply((SCMProcedure)fn, args);
    }
    /* Call AFn via helper method (function in Java) */
    Object result = AFn.apply(fn, args);

    /* Evaluate forced promise */
    if (result instanceof SCMPromise) {
      result = evalForcedPromise((SCMPromise)result, env);
    }
    return result;
  }

  // TODO Move out of Evaluator into call/cc
  /* Actual call-with-current-continuation */
  private Object callcc(IFn proc, IEnvironment env) {
    Continuation cont = new Continuation();
    try {
      /* Pass Continuation to the Procedure: (proc cont) */
      return eval(SCMCons.list(proc, cont), env);
    } catch (CalledContinuation ex) {
      if (ex.getContinuation() != cont) {
        /* Not our continuation, throw it further */
        throw ex;
      }
      /* Our continuation, grab and return the resulting value */
      return ex.getValue();
    } finally {
      /* One-shot continuations cannot be used more than once */
      cont.invalidate();
    }
  }

  // TODO Move out of Evaluator into dynamic-wind
  /* Actual dynamic-wind */
  private Object dynamicWind(IFn pre, IFn value, IFn post, IEnvironment env) {
    /* Evaluate before-thunk first */
    eval(SCMCons.list(pre), env);
    try {
    /* Evaluate and return value-thunk */
      return eval(SCMCons.list(value), env);
    } finally {
      /* Finally, evaluate post-thunk */
      eval(SCMCons.list(post), env);
    }
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
    try {
      /* Evaluate the body */
      Object result = eval(promise.getBody(), env);
      /* Mark Promise as FULFILLED */
      promise.setState(SCMPromise.State.FULFILLED);
      /* Memoize the result */
      promise.setResult(result);
      return result;
    } catch (Exception e) {
      /* Mark Promise as REJECTED */
      promise.setState(SCMPromise.State.REJECTED);
      /* Memoize the result */
      promise.setResult(e);
      throw e;
    }
  }
}
