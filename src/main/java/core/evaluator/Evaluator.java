package core.evaluator;

import core.environment.Environment;
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

public class Evaluator {

  /* Macroexpand S-expression, evaluate it and then return the result */
  public Object macroexpandAndEvaluate(Object sexp, Environment env) {
    return eval(macroexpand(sexp), env);
  }

  // TODO Implement
  private Object macroexpand(Object sexp) {
    return sexp;
  }

  public Object eval(Object sexp, Environment env) {
    /* TCO: This is our Trampoline */
    Object result;
    try {
      result = evalIter(sexp, env);
      while (result instanceof SCMTailCall) {
        Environment context = ((SCMTailCall) result).getContext();
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
  private Object evalIter(Object sexp, Environment env) {
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
  private Object evlis(List<Object> sexp, Environment env) {
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
      SCMProcedure proc = (SCMProcedure)fn;
      /* Bind args and put them into new local environment */
      Environment localEnvironment = proc.bindArgs(args);
      return evlis(proc.getBody(), localEnvironment);
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
  private Object callcc(IFn proc, Environment env) {
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
  private Object dynamicWind(IFn pre, IFn value, IFn post, Environment env) {
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
   * Evaluate forced Promise
   */
  private Object evalForcedPromise(SCMPromise promise, Environment env) {
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
