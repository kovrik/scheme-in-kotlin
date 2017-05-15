package core.procedures.hashmaps;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.Map;

public final class Key extends AFn {

  public Key() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Map.Entry.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "key";
  }

  @Override
  public Object apply1(Object arg) {
    return ((Map.Entry)arg).getKey();
  }
}
