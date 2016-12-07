package core.evaluator;

import core.environment.Environment;
import core.environment.IEnvironment;
import core.exceptions.ArityException;
import core.exceptions.IllegalSyntaxException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.IFn;
import core.scm.FnArgs;
import core.scm.SCMClass;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMPromise;
import core.scm.SCMSymbol;
import core.scm.specialforms.ISpecialForm;
import core.scm.SCMTailCall;
import core.writer.Writer;

import java.util.ArrayList;
import java.util.List;

public class Evaluator implements IEvaluator {

  @Override
  public Object eval(Object sexp, IEnvironment env) {
    /* TCO: This is our Trampoline */
    Object result = evalIter(sexp, env);
    while (result instanceof SCMTailCall) {
      IEnvironment context = ((SCMTailCall) result).getContext();
      if (context == null) {
        context = env;
      }
      result = evalIter(((SCMTailCall)result).getExpr(), context);
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

    /* Check args size */
    Class<?>[] fnArgs = null;
    FnArgs annotation = null;
    if (fn.getClass().isAnnotationPresent(FnArgs.class)) {
      annotation = fn.getClass().getAnnotation(FnArgs.class);
      fnArgs = annotation.args();
      if (annotation.isVariadic()) {
        /* Mandatory args */
        if (fnArgs.length > sexp.size() - 1) {
          throw new ArityException(sexp.size() - 1, ((AFn) fn).getName());
        }
      } else {
        if (fnArgs.length != sexp.size() - 1) {
          throw new ArityException(sexp.size() - 1, fnArgs.length, ((AFn) fn).getName());
        }
      }
    }

    /* Evaluate arguments first (because applicative order) and check their types */
    List<Object> args = new ArrayList<>(sexp.size() - 1);
    for (int i = 1; i < sexp.size(); i++) {
      Object arg = eval(sexp.get(i), env);
      args.add(arg);
      if (annotation != null) {
        if (annotation.isVariadic() && (fnArgs.length < i)) {
          continue;
        }
        /* FIXME Workaround for SCM Lists and Pairs: check and replace with marker class at Runtime */
        Class<?> actualClass = arg.getClass();
        if ((fnArgs[i - 1].equals(SCMCons.SCMProperList.class)) && (SCMCons.isList(arg))) {
          actualClass = SCMCons.SCMProperList.class;
        } else if ((fnArgs[i - 1].equals(SCMCons.SCMPair.class)) && (SCMCons.isPair(arg))) {
          actualClass = SCMCons.SCMPair.class;
        }
        if (!(SCMClass.checkClass(fnArgs[i - 1], actualClass))) {
          throw new WrongTypeException(Writer.write(fnArgs[i - 1]), arg);
        }
      }
    }

    /* Scheme procedure (lambda) */
    if (fn instanceof SCMProcedure) {
      return apply((SCMProcedure)fn, args);
    }

    /* IFn (function in Java) */
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
