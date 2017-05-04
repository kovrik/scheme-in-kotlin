package core.procedures.hashmaps;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.Cons;

import java.util.Map;

public final class Keys extends AFn {

  public Keys() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Map.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "keys";
  }

  @Override
  public Object apply1(Object arg) {
    return Cons.list(((Map)arg).keySet());
  }
}
