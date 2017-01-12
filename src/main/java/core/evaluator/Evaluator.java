package core.evaluator;

import core.environment.Environment;
import core.exceptions.IllegalSyntaxException;
import core.exceptions.ReentrantContinuationException;
import core.procedures.AFn;
import core.procedures.IFn;
import core.procedures.continuations.CallCC;
import core.procedures.continuations.CalledContinuation;
import core.procedures.continuations.DynamicWind;
import core.scm.*;
import core.scm.specialforms.ISpecialForm;
import core.utils.NumberUtils;
import core.writer.Writer;

import java.math.BigDecimal;
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
      while (result instanceof SCMThunk) {
        Environment context = ((SCMThunk) result).getContext();
        if (context == null) {
          context = env;
        }
        result = evalIter(((SCMThunk) result).getExpr(), context);
      }
    } catch (CalledContinuation cc) {
      if (cc.getContinuation().isInvoked()) {
        /* We have one-shot continuations only, not full continuations.
         * It means that we can't use the same continuation multiple times. */
        throw new ReentrantContinuationException();
      }
      /* Continuation is still valid, rethrow it further (should be caught by callcc)  */
      throw cc;
    }
    /* Try to downcast big numbers */
    if (result instanceof BigDecimal) {
      result = NumberUtils.tryToDowncast((BigDecimal) result);
    }
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
      return ((CallCC)fn).callcc((IFn) args.get(0), env, this);
    }
    /* dynamic-wind */
    if (fn instanceof DynamicWind) {
      return ((DynamicWind)fn).dynamicWind((IFn)args.get(0), (IFn)args.get(1), (IFn)args.get(2), env, this);
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
      result = ((SCMPromise)result).force(env, this);
    }
    return result;
  }
}
