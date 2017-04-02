package core.evaluator;

import core.environment.Environment;
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
import core.utils.NumberUtils;
import core.writer.Writer;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
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
        return evalJavaStaticField(sexp.toString());
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
    if (op instanceof SCMSymbol) {
      /* Lookup symbol */
      op = env.findOrDefault(op, null);
      if (op == null) {
        return evalJavaMethod(sexp);
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
      if (!(op instanceof AFn)) {
        throw new IllegalArgumentException("Wrong type to apply: " + Writer.write(op));
      }
    }
    AFn fn = (AFn)op;

    /* Scheme has applicative order, so evaluate all arguments first */
    List<Object> args = new ArrayList<>(sexp.size() - 1);
    for (int i = 1; i < sexp.size(); i++) {
      args.add(eval(sexp.get(i), env));
    }
    // TODO Turn them into Special Forms?
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

  private Class getClass(String name) {
    if (name.indexOf('.') == -1) {
      name = "java.lang." + name;
    }
    try {
      return Class.forName(name);
    } catch (ClassNotFoundException e) {
      throw new IllegalSyntaxException("class not found: " + name);
    }
  }

  // TODO Overloaded method resolution
  // TODO Native methods? (.getClass)
  private Method getMethod(Class clazz, String name, Class<?>... parameterTypes) {
    try {
      return clazz.getMethod(name, parameterTypes);
    } catch (NoSuchMethodException e) {
      throw new IllegalSyntaxException(String.format("method %s not found in class %s", name, clazz.getName()));
    }
  }

  private Object evalJavaStaticField(String s) {
    /* Java Interop: static fields */
    if (s.indexOf('/') > -1) {
      String[] classAndField = s.split("/");
      String className = classAndField[0];
      String field = classAndField[1];
      Class c = getClass(className);
      try {
        return c.getField(field).get(c);
      } catch (NoSuchFieldException e) {
        throw new IllegalSyntaxException(String.format("unable to find static field %s in class %s", field, className));
      } catch (IllegalAccessException e) {
        throw new IllegalSyntaxException(String.format("unable to access static field %s in class %s", field, className));
      }
    }
    throw new IllegalArgumentException("undefined identifier: " + s);
  }

  // TODO Move reflection to a separate class
  // TODO get instance field value
  private Object evalJavaMethod(List<Object> sexp) {
    Object op = sexp.get(0);
    /* Java Interop: instance method call */
    String m = op.toString();
    if (m.indexOf('.') == 0) {
      String methodName = m.substring(1);
      Object o = sexp.get(1);
      String name = o.toString();
      Class<?> clazz;
      boolean isClass = false;
      if (Character.isUpperCase(name.substring(name.lastIndexOf('.') + 1).charAt(0))) {
        clazz = getClass(name);
        isClass = true;
      } else {
        clazz = o.getClass();
      }
      Object[] args = sexp.subList(2, sexp.size()).toArray();
      Class[] argTypes = new Class[args.length];
      for (int i = 0; i < args.length; i++) {
        argTypes[i] = args[i].getClass();
      }
      Method method = getMethod(isClass ? Class.class : clazz, methodName, argTypes);
      try {
        return method.invoke(isClass ? clazz : o, args);
      } catch (IllegalAccessException e) {
        throw new IllegalSyntaxException(String.format("unable to access method %s of %s", methodName, o));
      } catch (InvocationTargetException e) {
        e.printStackTrace();
      }
    }
    /* Java Interop: static method call */
    if (m.indexOf('/') != -1) {
      String[] classAndMethod = m.split("/");
      String className = classAndMethod[0];
      String methodName = classAndMethod[1];
      Class clazz = getClass(className);
      Object[] args = sexp.subList(1, sexp.size()).toArray();
      Class[] argTypes = new Class[args.length];
      for (int i = 0; i < args.length; i++) {
        argTypes[i] = args[i].getClass();
      }
      Method method = getMethod(clazz, methodName, argTypes);
      try {
        return method.invoke(null, args);
      } catch (IllegalAccessException e) {
        throw new IllegalSyntaxException(String.format("unable to access static method %s of %s", methodName, clazz.getName()));
      } catch (InvocationTargetException e) {
        e.printStackTrace();
      }
    }
    throw new IllegalArgumentException("undefined identifier: " + op);
  }
}
