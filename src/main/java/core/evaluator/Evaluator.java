package core.evaluator;

import core.environment.Environment;
import core.environment.IEnvironment;
import core.exceptions.ArityException;
import core.exceptions.IllegalSyntaxException;
import core.exceptions.ReentrantContinuationException;
import core.exceptions.WrongTypeException;
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
    /* Check if op is a Special Form.
     * This is used for implicit Special Forms
     * used in other Special Forms (like BEGIN in LAMBDA).
     * We should be able to eval such forms even if user
     * has redefined implicit Special Form */
    Object op = sexp.get(0);
    if (op instanceof ISpecialForm) {
      return ((ISpecialForm)op).eval(sexp, env, this);
    }
    /* Check if Symbol refers to a Special Form */
    if (op instanceof SCMSymbol) {
      /* Get it from the environment: let user redefine special forms */
      Object specialForm = env.find(op);
      if (specialForm instanceof ISpecialForm) {
        // TODO Check if we can actually do this!
        /* Inline Special Form */
        sexp.set(0, specialForm);
        return ((ISpecialForm)specialForm).eval(sexp, env, this);
      }
    }

    /* Must be a procedure */
    Object fn;
    /* If procedure was inlined, then just apply it */
    if ((op instanceof AFn) && !(op instanceof SCMProcedure) ) {
      fn = op;
    } else {
      fn = eval(op, env);
      if (!(fn instanceof AFn)) {
        /* Can apply IFn only */
        throw new IllegalArgumentException("Wrong type to apply: " + Writer.write(fn));
      }
    }

    // TODO Check if we can actually do this!
    /* Inline pure fns tp avoid further lookups */
    if (((AFn)fn).isPure())  {
      sexp.set(0, fn);
    }

    /* Check args size (if FnArgs annotation is present) */
    FnArgs fnArgs = null;
    Class<?>[] mandatoryArgsTypes = null;
    Class<?> restArgsType = null;
    Class<?> lastArgType = null;
    if (fn.getClass().isAnnotationPresent(FnArgs.class)) {
      fnArgs = fn.getClass().getAnnotation(FnArgs.class);
      mandatoryArgsTypes = fnArgs.mandatoryArgsTypes();
      if (fnArgs.restArgsType().length > 0) {
        restArgsType = fnArgs.restArgsType()[0];
      }
      if (fnArgs.lastArgType().length > 0) {
        lastArgType = fnArgs.lastArgType()[0];
      }
      int actualArgCount = sexp.size() - 1;
      if (actualArgCount < fnArgs.minArgs()) {
        throw new ArityException(actualArgCount, fnArgs.minArgs(), ((AFn) fn).getName());
      }
      if (actualArgCount > fnArgs.minArgs() && (fnArgs.minArgs() == fnArgs.maxArgs())) {
        throw new ArityException(actualArgCount, fnArgs.minArgs(), ((AFn) fn).getName());
      }
      if (actualArgCount > fnArgs.maxArgs()) {
        throw new ArityException(actualArgCount, ((AFn) fn).getName());
      }
    }

    /* Evaluate arguments first (because applicative order) and check their types */
    List<Object> args = new ArrayList<>(sexp.size() - 1);
    for (int i = 1; i < sexp.size(); i++) {
      Object arg = eval(sexp.get(i), env);
      args.add(arg);

      if (fnArgs != null) {
        /* Mandatory args */
        if (mandatoryArgsTypes.length > 0 && i <= mandatoryArgsTypes.length) {
          if (!(SCMClass.checkType(arg, mandatoryArgsTypes[i - 1]))) {
            throw new WrongTypeException(Writer.write(mandatoryArgsTypes[i - 1]), arg);
          }
          continue;
        }
        /* Last argument (optional special case) */
        if (i == sexp.size() - 1 && (lastArgType != null)) {
          if (!(SCMClass.checkType(arg, lastArgType))) {
            throw new WrongTypeException(Writer.write(lastArgType), arg);
          }
          continue;
        }
        /* Rest args */
        if (restArgsType != null) {
          if (!(SCMClass.checkType(arg, restArgsType))) {
            throw new WrongTypeException(Writer.write(restArgsType), arg);
          }
        }
      }
    }

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
    Object result = AFn.apply((AFn)fn, args, fnArgs);

    /* Handle Promise forced to evaluation by Force procedure */
    if ((result instanceof SCMPromise) && ((SCMPromise)result).getState() == SCMPromise.State.FORCED) {
      result = evalForcedPromise((SCMPromise)result, env);
    }
    return result;
  }

  // FIXME Move out of Evaluator into call/cc
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

  // FIXME Move out of Evaluator into dynamic-wind
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
