package core.procedures.exceptions;

import core.exceptions.ExInfoException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.Map;

public class ExInfo extends AFn {

  public ExInfo() {
    super(new FnArgsBuilder().minArgs(2).maxArgs(3).mandatoryArgsTypes(new Class[]{String.class, Map.class}).restArgsType(Throwable.class));
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
  public Map apply(Object... args) {
    if (args.length == 2) {
      throw new ExInfoException((String)args[0], (Map)args[1]);
    }
    throw new ExInfoException((String)args[0], (Map)args[1], (Throwable)args[2]);
  }
}
