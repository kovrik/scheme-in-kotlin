package core.evaluator;

import core.environment.Environment;
import core.exceptions.ArityException;
import core.exceptions.IllegalSyntaxException;
import core.exceptions.ReentrantContinuationException;
import core.procedures.AFn;
import core.procedures.IFn;
import core.procedures.continuations.CallCC;
import core.procedures.continuations.CalledContinuation;
import core.procedures.continuations.DynamicWind;
import core.procedures.delayed.Force;
import core.scm.SCMBigRational;
import core.scm.SCMPromise;
import core.scm.SCMSymbol;
import core.scm.SCMThunk;
import core.scm.specialforms.ISpecialForm;
import core.scm.specialforms.New;
import core.utils.NumberUtils;
import core.writer.Writer;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Evaluator {

  private final Reflector reflector = new Reflector();

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
        result = evalIter(((SCMThunk) result).getExpr(),
                          ((SCMThunk) result).getContextOrDefault(env));
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
    /* Do not downcast in case of `new` Special Form (workaround) */
    if (result instanceof New.NewInstanceResult) {
      return ((New.NewInstanceResult) result).getInstance();
    }
    /* Try to downcast Big Numbers */
    if (result instanceof BigDecimal) {
      result = NumberUtils.tryToDowncast((BigDecimal) result);
    }
    /* Try to downcast Rationals */
    if ((result instanceof SCMBigRational) && (((SCMBigRational) result).isDenominatorEqualToOne())) {
      result = NumberUtils.tryToDowncast(((SCMBigRational) result).getNumerator());
    }
    return result;
  }

  /**
   * One iteration of evaluation.
   * Returns the end result or a Thunk object.
   * If Thunk is returned, then eval() method (trampoline) continues evaluation.
   */
  private Object evalIter(Object sexp, Environment env) {
    if (sexp instanceof SCMSymbol) {
      /* Check if it is a Special Form */
      Object o = env.findOrDefault(sexp, null);
      if (o instanceof ISpecialForm) {
        throw IllegalSyntaxException.of(o.toString(), sexp);
      }
      if (o == null) {
        return reflector.evalJavaStaticField(sexp.toString());
      }
      return o;
    } else if (sexp instanceof List) {
      return evlis((List<Object>)sexp, env);
    } else if (sexp instanceof Map) {
      return evalMap((Map)sexp, env);
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

    boolean javaMethod = false;
    Object op = sexp.get(0);
    if (op instanceof SCMSymbol) {
      /* Lookup symbol */
      op = env.findOrDefault(op, null);
      // TODO Check if op starts with '.' instead?
      javaMethod = op == null;
      if (javaMethod && sexp.size() < 2) {
        throw IllegalSyntaxException.of("eval", sexp, "illegal member expression");
      }
      /* Inline Special Forms and Pure functions */
      if (op instanceof ISpecialForm || ((op instanceof AFn) && (((AFn) op).isPure()))) {
        sexp.set(0, op);
      }
    }

    /* If it is a Special Form, then evaluate it */
    if (op instanceof ISpecialForm) {
      return ((ISpecialForm)op).eval(sexp, env, this);
    }

    /* Maps like as functions of their keys */
    if (op instanceof Map) {
      if (sexp.size() > 3) {
        throw new ArityException("hashmap", 1, 2, sexp.size() - 1);
      }
      Map map = evalMap((Map)op, env);
      /* Evaluate key */
      Object key = eval(sexp.get(1), env);
      Object defaultValue = null;
      if (sexp.size() == 3) {
        defaultValue = eval(sexp.get(2), env);
      }
      return map.getOrDefault(key, defaultValue);
    }

    /* If it is not AFn, then try to evaluate it (assuming it is a Lambda) */
    if (!(op instanceof AFn)) {
      op = eval(op, env);
      /* If result is not a function, then raise an error */
      if (!(op instanceof AFn) && !javaMethod) {
        throw new IllegalArgumentException("Wrong type to apply: " + Writer.write(op));
      }
    }

    /* Scheme has applicative order, so evaluate all arguments first */
    List<Object> args = new ArrayList<>(sexp.size() - 1);
    if (javaMethod) {
      String method = sexp.get(0).toString();
      /* Check if it is instance or static method */
      int n = 2;
      if (sexp.get(1) instanceof SCMSymbol) {
        args.add(env.findOrDefault(sexp.get(1), sexp.get(1)));
      } else {
        args.add(eval(sexp.get(1), env));
      }

      for (int i = n; i < sexp.size(); i++) {
        args.add(eval(sexp.get(i), env));
      }
      return reflector.evalJavaMethod(method, args.toArray());
    }

    /* Evaluate args */
    for (int i = 1; i < sexp.size(); i++) {
      args.add(eval(sexp.get(i), env));
    }

    // TODO Turn them into Special Forms?
    AFn fn = (AFn)op;
    /* force */
    if (fn instanceof Force) {
      return ((Force)fn).force((SCMPromise)args.get(0), env, this);
    }
    /* call-with-current-continuation */
    if (fn instanceof CallCC) {
      return ((CallCC)fn).callcc((IFn) args.get(0), env, this);
    }
    /* dynamic-wind */
    if (fn instanceof DynamicWind) {
      return ((DynamicWind)fn).dynamicWind((IFn)args.get(0), (IFn)args.get(1), (IFn)args.get(2), env, this);
    }
    /* Call AFn via helper method */
    return fn.applyN(args);
  }

  /* Evaluate hash map */
  private Map evalMap(Map<Object, Object> map, Environment env) {
    Map<Object, Object> result = new HashMap<>(map.size());
    for (Map.Entry<Object, Object> entry : map.entrySet()) {
      Object key = eval(entry.getKey(), env);
      Object value = eval(entry.getValue(), env);
      result.put(key, value);
    }
    return result;
  }
}
