package core.procedures.sets;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.HashSet;
import java.util.Set;

public final class Difference extends AFn {

  public Difference() {
    super(new FnArgsBuilder().minArgs(1).mandatoryArgsTypes(new Class[]{Set.class}).restArgsType(Set.class));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "difference";
  }

  @Override
  public Set<Object> apply(Object... args) {
    if (args.length == 1) {
      return (Set<Object>) args[0];
    }
    Set result = new HashSet((Set)args[0]);
    for (int i = 1, argsLength = args.length; i < argsLength; i++) {
      result.removeAll((Set)args[i]);
    }
    return result;
  }
}
