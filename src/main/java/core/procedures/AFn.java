package core.procedures;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.scm.FnArgs;
import core.scm.SCMClass;
import core.writer.Writer;

import java.util.List;

/* Abstract superclass of all functions */
@FnArgs
public abstract class AFn implements IFn<Object[], Object> {

  /* Default FnArgs annotation instance */
  private static final FnArgs DEFAULT = AFn.class.getAnnotation(FnArgs.class);

  /* Save FnArgs annotation of current class */
  private final FnArgs fnArgs = (getClass().isAnnotationPresent(FnArgs.class)) ?
                                 getClass().getAnnotation(FnArgs.class) : DEFAULT;

  public int minArgs() {
    return fnArgs.minArgs();
  }

  public int maxArgs() {
    return fnArgs.maxArgs();
  }

  public Class<?>[] mandatoryArgsTypes() {
    return fnArgs.mandatoryArgsTypes();
  }

  public Class<?>[] restArgsType() {
    return fnArgs.restArgsType();
  }

  public Class<?>[] lastArgType() {
    return fnArgs.lastArgType();
  }

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
  public final void checkArgs(List<Object> args) {
    /* Check arg count */
    int actualArgCount = args.size();
    if (actualArgCount < minArgs()) {
      throw new ArityException(actualArgCount, minArgs(), getName(), minArgs() != maxArgs());
    }
    if (actualArgCount > minArgs() && (minArgs() == maxArgs())) {
      throw new ArityException(actualArgCount, minArgs(), getName(), minArgs() != maxArgs());
    }
    if (actualArgCount > maxArgs()) {
      throw new ArityException(actualArgCount, getName());
    }

    /* Get arg types */
    Class<?>[] mandatoryArgsTypes = mandatoryArgsTypes();
    Class<?> restArgsType = null;
    Class<?> lastArgType = null;
    if (restArgsType().length > 0) {
      restArgsType = restArgsType()[0];
    }
    if (lastArgType().length > 0) {
      lastArgType = lastArgType()[0];
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

  /**
   * Helper method that checks if FnArgs annotation is present,
   * if function is a fixed-arity function and if it is,
   * then calls applyN() methods (where N is arity).
   * Calls variadic apply() otherwise.
   */
  public final Object applyN(List<Object> args) {
    /* if minArgs == maxArgs, then function is not variadic, hence get arity */
    int arity = (minArgs() == maxArgs()) ? minArgs() : -1;
    switch (arity) {
      case 0:  return apply0();
      case 1:  return apply1(args.get(0));
      case 2:  return apply2(args.get(0), args.get(1));
      case 3:  return apply3(args.get(0), args.get(1), args.get(2));
      case 4:  return apply4(args.get(0), args.get(1), args.get(2), args.get(3));
      default: return apply(args.toArray());
    }
  }
}
