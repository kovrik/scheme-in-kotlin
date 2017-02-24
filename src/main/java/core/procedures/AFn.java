package core.procedures;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.scm.FnArgs;
import core.scm.SCMClass;
import core.writer.Writer;

import java.util.List;

/* Abstract superclass of all functions */
public abstract class AFn implements IFn<Object[], Object> {

  private final int minArgs;
  private final int maxArgs;
  private final Class<?>[] mandatoryArgsTypes;
  private final Class<?>[] restArgsType;
  private final Class<?>[] lastArgType;

  public AFn() {
    if (getClass().isAnnotationPresent(FnArgs.class)) {
      FnArgs fnArgs = getClass().getAnnotation(FnArgs.class);
      minArgs = fnArgs.minArgs();
      maxArgs = fnArgs.maxArgs();
      mandatoryArgsTypes = fnArgs.mandatoryArgsTypes();
      restArgsType = fnArgs.restArgsType();
      lastArgType = fnArgs.lastArgType();
    } else {
      minArgs = 0;
      maxArgs = 255;
      mandatoryArgsTypes = new Class<?>[]{};
      restArgsType = new Class<?>[]{};
      lastArgType = new Class<?>[]{};
    }
  }

  public AFn(FnArgsBuilder fnArgsBuilder) {
    this.minArgs = fnArgsBuilder.getMinArgs();
    this.maxArgs = fnArgsBuilder.getMaxArgs();
    this.mandatoryArgsTypes = fnArgsBuilder.getMandatoryArgsTypes();
    this.restArgsType = fnArgsBuilder.getRestArgsType();
    this.lastArgType = fnArgsBuilder.getLastArgType();
  }

  public int minArgs() {
    return minArgs;
  }

  public int maxArgs() {
    return maxArgs;
  }

  public Class<?>[] mandatoryArgsTypes() {
    return mandatoryArgsTypes;
  }

  public Class<?>[] restArgsType() {
    return restArgsType;
  }

  public Class<?>[] lastArgType() {
    return lastArgType;
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
    String name = getName();
    if (name == null || name.isEmpty()) {
      return "#<procedure>";
    }
    return "#<procedure:" + name + ">";
  }

  /**
   * Checks the number of arguments and their types
   * (if function is annotated with FnArgs)
   */
  private void checkArgs(List<Object> args) {
    /* Check arg count */
    int argsSize = args.size();
    if (argsSize < minArgs) {
      throw new ArityException(argsSize, minArgs, getName(), minArgs != maxArgs);
    }
    if (argsSize > minArgs && (minArgs == maxArgs)) {
      throw new ArityException(argsSize, minArgs, getName(), minArgs != maxArgs);
    }
    if (argsSize > maxArgs) {
      throw new ArityException(argsSize, getName());
    }

    /* Get arg types */
    Class<?> restType = null;
    Class<?> lastType = null;
    if (restArgsType.length > 0) {
      restType = restArgsType[0];
    }
    if (lastArgType().length > 0) {
      lastType = lastArgType[0];
    }

    /* Now check arg types (if function is annotated with FnArgs */
    for (int i = 0; i < argsSize; i++) {
      Object arg = args.get(i);
      /* Mandatory args */
      if (mandatoryArgsTypes.length > 0 && i < mandatoryArgsTypes.length) {
        if (!(SCMClass.checkType(arg, mandatoryArgsTypes[i]))) {
          throw new WrongTypeException(Writer.write(mandatoryArgsTypes[i]), arg);
        }
        continue;
      }
      /* Last argument (optional special case) */
      if (i == argsSize - 1 && (lastType != null)) {
        if (!(SCMClass.checkType(arg, lastType))) {
          throw new WrongTypeException(Writer.write(lastType), arg);
        }
        continue;
      }
      /* Rest args */
      if (restType != null) {
        if (!(SCMClass.checkType(arg, restType))) {
          throw new WrongTypeException(Writer.write(restType), arg);
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
    /* Check args */
    checkArgs(args);
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
