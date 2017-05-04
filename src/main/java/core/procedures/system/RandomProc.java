package core.procedures.system;

import core.exceptions.WrongTypeException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.concurrent.ThreadLocalRandom;

public final class RandomProc extends AFn {

  public RandomProc() {
    super(new FnArgsBuilder().max(2).rest(Long.class).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "random";
  }

  @Override
  public Object apply(Object... args) {
    if (args.length == 0) {
      return Math.random();
    }
    if (args.length == 1) {
      if (!(args[0] instanceof Long) || (((Long) args[0]) < 1) || ((Long) args[0] > Integer.MAX_VALUE)) {
        throw new WrongTypeException(getName(), String.format("Integer (1 to %s)", Integer.MAX_VALUE), args[0]);
      }
      return (long)(new java.util.Random().nextInt(((Long) args[0]).intValue()));
    }
    if (!(args[0] instanceof Long) || (((Long) args[0]) < 1) || ((Long) args[0] > Integer.MAX_VALUE)) {
      throw new WrongTypeException(getName(), String.format("Integer (1 to %s)", Integer.MAX_VALUE), args[0]);
    }
    if (!(args[1] instanceof Long) || (((Long) args[1]) < 1) || ((Long) args[1] > Integer.MAX_VALUE)) {
      throw new WrongTypeException(getName(), String.format("Integer (1 to %s)", Integer.MAX_VALUE), args[1]);
    }
    return (long)(ThreadLocalRandom.current().nextInt(((Long) args[0]).intValue(), ((Long) args[1]).intValue()));
  }
}
