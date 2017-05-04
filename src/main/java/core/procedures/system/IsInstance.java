package core.procedures.system;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

public class IsInstance extends AFn {

  public IsInstance() {
    super(new FnArgsBuilder().min(2).max(2).mandatory(new Class[]{Class.class, Object.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "instance?";
  }

  @Override
  public Boolean apply2(Object arg1, Object arg2) {
    return ((Class)arg1).isAssignableFrom(arg2.getClass());
  }
}
