package core.procedures.sets;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.HashSet;
import java.util.Set;

public final class Intersection extends AFn {

  public Intersection() {
    super(new FnArgsBuilder().min(1).mandatory(new Class[]{Set.class}).rest(Set.class).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "intersection";
  }

  @Override
  public Set<Object> apply(Object... args) {
    if (args.length == 1) {
      return (Set<Object>) args[0];
    }
    Set result = new HashSet((Set)args[0]);
    for (int i = 1, argsLength = args.length; i < argsLength; i++) {
      result.retainAll((Set)args[i]);
    }
    return result;
  }
}
