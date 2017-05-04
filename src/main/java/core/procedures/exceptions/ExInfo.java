package core.procedures.exceptions;

import core.exceptions.ExInfoException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.Map;

public class ExInfo extends AFn {

  public ExInfo() {
    super(new FnArgsBuilder().min(2).max(3).mandatory(new Class[]{String.class, Map.class}).rest(Throwable.class).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "ex-info";
  }

  @Override
  public ExInfoException apply(Object... args) {
    if (args.length == 2) {
      return new ExInfoException((String)args[0], (Map)args[1]);
    }
    return new ExInfoException((String)args[0], (Map)args[1], (Throwable)args[2]);
  }
}
