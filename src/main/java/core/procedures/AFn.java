package core.procedures;

import core.exceptions.ArityException;
import core.scm.FnArgs;

import java.util.List;

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
    String name = getName();
    if (name == null || name.isEmpty()) {
      return "#<procedure>";
    }
    return "#<procedure:" + name + ">";
  }

  public static Object apply(AFn fn, List<Object> args, FnArgs fnArgs) {
    if (fnArgs != null && fnArgs.minArgs() == fnArgs.maxArgs()) {
      switch (fnArgs.minArgs()) {
        case 0: return fn.apply0();
        case 1: return fn.apply1(args.get(0));
        case 2: return fn.apply2(args.get(0), args.get(1));
        case 3: return fn.apply3(args.get(0), args.get(1), args.get(2));
        case 4: return fn.apply4(args.get(0), args.get(1), args.get(2), args.get(3));
      }
    }
    return fn.apply(args.toArray());
  }
}
