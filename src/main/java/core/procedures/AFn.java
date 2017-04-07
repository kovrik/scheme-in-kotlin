package core.procedures;

import core.exceptions.ArityException;
import core.exceptions.WrongTypeException;
import core.scm.SCMClass;
import core.writer.Writer;

import java.util.List;

/* Abstract superclass of all functions */
public abstract class AFn implements IFn<Object[], Object> {

  private final int minArgs;
  private final int maxArgs;
  private final Class<?>[] mandatoryArgsTypes;
  private final Class<?> restArgsType;
  private final Class<?> lastArgType;

  public AFn() {
    minArgs = 0;
    maxArgs = 255;
    mandatoryArgsTypes = new Class<?>[]{};
    restArgsType = null;
    lastArgType = null;
  }

  protected AFn(FnArgsBuilder fnArgsBuilder) {
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

  /* Return true if function is pure (referentially transparent) */
  public boolean isPure() {
    return false;
  }

  public Object apply0() {
    throw new ArityException(getName(), minArgs, maxArgs, 1);
  }

  public Object apply1(Object arg) {
    throw new ArityException(getName(), minArgs, maxArgs, 1);
  }

  public Object apply2(Object arg1, Object arg2) {
    throw new ArityException(getName(), minArgs, maxArgs, 2);
  }

  public Object apply3(Object arg1, Object arg2, Object arg3) {
    throw new ArityException(getName(), minArgs, maxArgs, 3);
  }

  public Object apply4(Object arg1, Object arg2, Object arg3, Object arg4) {
    throw new ArityException(getName(), minArgs, maxArgs, 4);
  }

  @Override
  public Object apply(Object... args) {
    throw new ArityException(getName(), minArgs, maxArgs, args.length);
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
   */
  private void checkArgs(List<Object> args) {
    /* Check arg count */
    int argsSize = args.size();
    if (argsSize < minArgs || argsSize > maxArgs) {
      throw new ArityException(getName(), minArgs, maxArgs, argsSize);
    }
    for (int i = 0; i < argsSize; i++) {
      Object arg = args.get(i);
      /* Mandatory args */
      if (mandatoryArgsTypes.length > 0 && i < mandatoryArgsTypes.length) {
        if (!(SCMClass.checkType(arg, mandatoryArgsTypes[i]))) {
          throw new WrongTypeException(getName(), Writer.writeClass(mandatoryArgsTypes[i]), arg);
        }
        continue;
      }
      /* Last argument (optional special case) */
      if (i == argsSize - 1 && (lastArgType != null)) {
        if (!(SCMClass.checkType(arg, lastArgType))) {
          throw new WrongTypeException(getName(), Writer.writeClass(lastArgType), arg);
        }
        continue;
      }
      /* Rest args */
      if (restArgsType != null) {
        if (!(SCMClass.checkType(arg, restArgsType))) {
          throw new WrongTypeException(getName(), Writer.writeClass(restArgsType), arg);
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
    int arity = (minArgs == maxArgs) ? minArgs : -1;
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
