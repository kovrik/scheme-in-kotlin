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
import core.scm.*;
import core.scm.specialforms.ISpecialForm;
import core.utils.NumberUtils;
import core.writer.Writer;

import java.math.BigDecimal;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicLong;

public class Evaluator {

  /* Executor Service for Futures */
  private static final AtomicLong threadCounter = new AtomicLong(0);
  public volatile static ExecutorService executor = Executors.newFixedThreadPool(2 + Runtime.getRuntime().availableProcessors(),
                                                                                 createThreadFactory(threadCounter));

  private static ThreadFactory createThreadFactory(AtomicLong threadCounter) {
    return r -> {
      Thread t = new Thread(r);
      t.setName("executor-thread-" + threadCounter.getAndIncrement());
      return t;
    };
  }

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
        result = evalIter(((SCMThunk) result).getExpr(), ((SCMThunk) result).getContextOrDefault(env));
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
    /* Try to downcast Big Numbers */
    if (result instanceof BigDecimal) {
      return NumberUtils.tryToDowncast((BigDecimal) result);
    }
    /* Try to downcast Rationals with denominator = 1 */
    if ((result instanceof SCMBigRational) && (((SCMBigRational) result).isDenominatorEqualToOne())) {
      return NumberUtils.tryToDowncast((SCMBigRational) result);
    }
    /* Now upcast number if required */
    if (result instanceof Number) {
      return maybeUpcast((Number) result);
    }
    // FIXME Get rid of this workaround
    /* Do not downcast in case of `new` Special Form (workaround) */
    if (result instanceof ReflectorResult) {
      return ((ReflectorResult) result).get();
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
        return ReflectorResult.maybeWrap(reflector.evalJavaStaticField(sexp.toString()));
      }
      /* Evaluate nil constant to null */
      if (o == SCMNil.NIL) {
        return null;
      }
      return o;
    } else if (sexp instanceof List) {
      return evlis((List<Object>)sexp, env);
    } else if (sexp instanceof Map) {
      return evalMap((Map)sexp, env);
    } else if (sexp instanceof SCMVector) {
      return evalVector((SCMVector)sexp, env);
    } else if (sexp instanceof Set) {
      return evalSet((Set)sexp, env);
    } else {
      /* Everything else evaluates to itself:
       * Numbers, Strings, Chars, Keywords etc. */
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
      if (javaMethod && sexp.isEmpty()) {
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

    /* If it is not AFn, then try to evaluate it (assuming it is a Lambda) */
    if (!(op instanceof AFn)) {
      op = eval(op, env);
      /* If result is not a function, then raise an error */
      if (!(op instanceof AFn) && !javaMethod && !(op instanceof Map) && !(op instanceof SCMVector)) {
        throw new IllegalArgumentException("Wrong type to apply: " + Writer.write(op));
      }
    }

    /* Maps are functions of their keys */
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

    /* Scheme has applicative order, so evaluate all arguments first */
    Object[] args = new Object[sexp.size() - 1];
    if (javaMethod) {
      String method = sexp.get(0).toString();
      if (sexp.size() > 1) {
        /* Check if it is instance or static method */
        args[0] = sexp.get(1) instanceof SCMSymbol ? env.findOrDefault(sexp.get(1), sexp.get(1)) : eval(sexp.get(1), env);
        for (int i = 2; i < sexp.size(); i++) {
          args[i - 1] = eval(sexp.get(i), env);
        }
      }
      return ReflectorResult.maybeWrap(reflector.evalJavaMethod(method, args));
    }

    /* Evaluate args */
    for (int i = 1; i < sexp.size(); i++) {
      args[i - 1] = eval(sexp.get(i), env);
    }

    // TODO Turn them into Special Forms?
    AFn fn = (AFn)op;
    /* call-with-current-continuation */
    if (fn instanceof CallCC) {
      return ((CallCC)fn).callcc((IFn) args[0], env, this);
    }
    /* dynamic-wind */
    if (fn instanceof DynamicWind) {
      return ((DynamicWind)fn).dynamicWind((IFn)args[0], (IFn)args[1], (IFn)args[2], env, this);
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

  /* Evaluate vector */
  private SCMVector evalVector(SCMVector vector, Environment env) {
    for (int i = 0; i < vector.length(); i++) {
      vector.getArray()[i] = eval(vector.getArray()[i], env);
    }
    return vector;
  }

  /* Evaluate set */
  private Set<Object> evalSet(Set<Object> set, Environment env) {
    Set<Object> result = new HashSet<>(set.size());
    for (Object e : set) {
      result.add(eval(e, env));
    }
    return result;
  }

  /* Upcast if required
   * (float to double, byte, short and int to long)
   */
  private Number maybeUpcast(Number number) {
    if ((number instanceof Byte) || (number instanceof Short) || (number instanceof Integer)) {
      return number.longValue();
    } else if (number instanceof Float) {
      return number.doubleValue();
    }
    return number;
  }
}
