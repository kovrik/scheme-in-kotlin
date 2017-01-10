package core.procedures;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.scm.FnArgs;
import core.scm.SCMClass;
import core.writer.Writer;

import java.util.List;

/* Abstract superclass of all functions */
public abstract class AFn implements IFn<Object[], Object> {

  /* Return true if function is pure (referentially transparent) */
  public boolean isPure() {
    return false;
  }

  public Object apply0() {
    throw new ArityException(0, getName());
  }

  public Object apply1(Object arg) {
    throw new ArityException(1, getName());
  }

  public Object apply2(Object arg1, Object arg2) {
    throw new ArityException(2, getName());
  }

  public Object apply3(Object arg1, Object arg2, Object arg3) {
    throw new ArityException(3, getName());
  }

  public Object apply4(Object arg1, Object arg2, Object arg3, Object arg4) {
    throw new ArityException(4, getName());
  }

  @Override
  public Object apply(Object... args) {
    throw new ArityException(args.length, getName());
  }

  public String getName() {
    return getClass().getSimpleName();
  }

  @Override
  public String toString() {
    return getName();
  }

  /**
   * Checks the number of arguments and their types
   * (if function is annotated with FnArgs)
   */
  public void checkArgs(List<Object> args) {
    /* Check if FnArgs annotation is present */
    if (getClass().isAnnotationPresent(FnArgs.class)) {
      FnArgs fnArgs = getClass().getAnnotation(FnArgs.class);
      /* Check arg count */
      int actualArgCount = args.size();
      if (actualArgCount < fnArgs.minArgs()) {
        throw new ArityException(actualArgCount, fnArgs.minArgs(), getName());
      }
      if (actualArgCount > fnArgs.minArgs() && (fnArgs.minArgs() == fnArgs.maxArgs())) {
        throw new ArityException(actualArgCount, fnArgs.minArgs(), getName());
      }
      if (actualArgCount > fnArgs.maxArgs()) {
        throw new ArityException(actualArgCount, getName());
      }

      /* Get arg types */
      Class<?>[] mandatoryArgsTypes = fnArgs.mandatoryArgsTypes();
      Class<?> restArgsType = null;
      Class<?> lastArgType = null;
      if (fnArgs.restArgsType().length > 0) {
        restArgsType = fnArgs.restArgsType()[0];
      }
      if (fnArgs.lastArgType().length > 0) {
        lastArgType = fnArgs.lastArgType()[0];
      }

      /* Now check arg types (if function is annotated with FnArgs */
      for (int i = 0; i < args.size(); i++) {
        Object arg = args.get(i);
        /* Mandatory args */
        if (mandatoryArgsTypes.length > 0 && i < mandatoryArgsTypes.length) {
          if (!(SCMClass.checkType(arg, mandatoryArgsTypes[i]))) {
            throw new WrongTypeException(Writer.write(mandatoryArgsTypes[i]), arg);
          }
          continue;
        }
        /* Last argument (optional special case) */
        if (i == args.size() - 1 && (lastArgType != null)) {
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
  }

  /**
   * Helper method that checks if FnArgs annotation is present,
   * if function is a fixed-arity function and if it is,
   * then calls applyN() methods (where N is arity).
   * Calls variadic apply() otherwise.
   */
  public static Object apply(AFn fn, List<Object> args) {
    /* Get FnArgs annotation if present */
    int arity = -1;
    if (fn.getClass().isAnnotationPresent(FnArgs.class)) {
      FnArgs fnArgs = fn.getClass().getAnnotation(FnArgs.class);
      /* if minArgs == maxArgs, then function is not variadic, hence get arity */
      arity = (fnArgs.minArgs() == fnArgs.maxArgs()) ? fnArgs.minArgs() : arity;
    }
    switch (arity) {
      case 0:  return fn.apply0();
      case 1:  return fn.apply1(args.get(0));
      case 2:  return fn.apply2(args.get(0), args.get(1));
      case 3:  return fn.apply3(args.get(0), args.get(1), args.get(2));
      case 4:  return fn.apply4(args.get(0), args.get(1), args.get(2), args.get(3));
      default: return fn.apply(args.toArray());
    }
  }
}
