package core.evaluator;

import core.environment.Environment;
import core.exceptions.ArityException;
import core.exceptions.IllegalSyntaxException;
import core.exceptions.ReentrantContinuationException;
import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.continuations.CalledContinuation;
import core.scm.BigRatio;
import core.scm.Cons;
import core.scm.MapEntry;
import core.scm.Symbol;
import core.scm.Thunk;
import core.scm.Vector;
import core.scm.specialforms.ISpecialForm;
import core.scm.specialforms.New;
import core.utils.Utils;
import core.writer.Writer;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
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
      while (result instanceof Thunk) {
        result = evalIter(((Thunk) result).getExpr(), ((Thunk) result).getContextOrDefault(env));
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
    if (result instanceof BigRatio) {
      return Utils.downcastNumber((Number)result);
    }
    return result;
  }

  /**
   * One iteration of evaluation.
   * Returns the end result or a Thunk object.
   * If Thunk is returned, then eval() method (trampoline) continues evaluation.
   */
  private Object evalIter(Object sexp, Environment env) {
    if (sexp instanceof Symbol) {
      return evalSymbol((Symbol)sexp, env);
    } else if (sexp instanceof List) {
      return evlis((List<Object>)sexp, env);
    } else if (sexp instanceof Map) {
      return evalMap((Map)sexp, env);
    } else if (sexp instanceof Vector) {
      return evalVector((Vector)sexp, env);
    } else if (sexp instanceof Set) {
      return evalSet((Set)sexp, env);
    } else {
      /* Everything else evaluates to itself:
       * Numbers, Strings, Chars, Keywords etc. */
      return sexp;
    }
  }

  /* Evaluate Symbol */
  private Object evalSymbol(Symbol symbol, Environment env) {
    /* Check if it is a Special Form */
    Object o = env.findOrDefault(symbol, Environment.UNDEFINED);
    if (o instanceof ISpecialForm) {
      throw IllegalSyntaxException.of(o.toString(), symbol);
    }
    if (o == Environment.UNDEFINED) {
      /* Check if it is a Java class. If not found, then assume it is a static field */
      Class clazz = reflector._getClass(symbol.getName());
      return clazz != null ? clazz : reflector.evalJavaStaticField(symbol.toString());
    }
    return o;
  }

  /* Evaluate list */
  private Object evlis(List<Object> sexp, Environment env) {
    if (sexp.isEmpty()) {
      throw IllegalSyntaxException.of("eval", sexp, "illegal empty application");
    }

    boolean javaMethod = false;
    Object op = sexp.get(0);
    if (op instanceof Symbol) {
      Symbol sym = (Symbol) op;
      /* Lookup symbol */
      op = env.findOrDefault(sym, Environment.UNDEFINED);
      /* Inline Special Forms and Pure functions */
      if (op instanceof ISpecialForm) {
        sexp.set(0, op);
      } else if (op instanceof AFn) {
        if (((AFn) op).isPure()) {
          sexp.set(0, op);
        }
      } else if (op == Environment.UNDEFINED) {
        // TODO Check if op starts with '.' instead?
        javaMethod = true;
        /* Special case: constructor call If Symbol ends with . */
        if (sym.getName().charAt(sym.getName().length() - 1) == '.') {
          // TODO Optimize and cleanup
          sexp.set(0, Symbol.intern(sym.getName().substring(0, sym.getName().length() - 1)));
          op = New.NEW;
          ((Cons) sexp).push(op);
        }
      }
    }

    /* If it is a Special Form, then evaluate it */
    if (op instanceof ISpecialForm) {
      return ((ISpecialForm)op).eval(sexp, env, this);
    }

    /* If it is not AFn, then try to evaluate it (assuming it is a Lambda) */
    if (!(op instanceof AFn)) {
      op = eval(op, env);
    }

    /* Vectors and Map Entries are functions of index */
    if (op instanceof Map.Entry) {
      /* Convert Map Entry into a MapEntry */
      op = new MapEntry((Map.Entry) op);
    }
    if (op instanceof Vector) {
      if (sexp.size() > 2) throw new ArityException("vector", 1, 1, sexp.size() - 1);
      Vector vector = evalVector((Vector)op, env);
      /* Evaluate key */
      Object key = eval(sexp.get(1), env);
      if (!Utils.isInteger(key)) {
        throw new WrongTypeException("vector", Integer.class, key);
      }
      int i = ((Number) key).intValue();
      if (vector.size() <= i || i < 0) {
        throw new IndexOutOfBoundsException(String.format("%s: value out of range: %s", vector, i));
      }
      return vector.get(i);
    } else if (op instanceof Map) {
      /* Maps are functions of their keys */
      if (sexp.size() > 3) throw new ArityException("hashmap", 1, 2, sexp.size() - 1);
      Map map = evalMap((Map)op, env);
      /* Evaluate key */
      Object key = eval(sexp.get(1), env);
      Object defaultValue = sexp.size() == 3 ? eval(sexp.get(2), env) : null;
      return map.getOrDefault(key, defaultValue);
    }

    /* If result is not a function, then raise an error */
    if (!(op instanceof AFn) && !javaMethod) {
      throw new IllegalArgumentException("wrong type to apply: " + Writer.write(op));
    }

    /* Scheme has applicative order, so evaluate all arguments first */
    Object[] args = new Object[sexp.size() - 1];
    for (int i = 1; i < sexp.size(); i++) {
      args[i - 1] = eval(sexp.get(i), env);
    }

    /* Call Java method */
    if (javaMethod) {
      String method = sexp.get(0).toString();
      return reflector.evalJavaMethod(method, args);
    }
    /* Call AFn via helper method */
    return ((AFn)op).applyN(args);
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
  private Vector evalVector(Vector vector, Environment env) {
    for (int i = 0; i < vector.size(); i++) {
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
}
