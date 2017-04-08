package core.procedures.sets;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.HashSet;
import java.util.Set;

public final class Union extends AFn {

  public Union() {
    super(new FnArgsBuilder().minArgs(0).restArgsType(Set.class));
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "union";
  }

  @Override
  public Set<Object> apply(Object... args) {
    if (args.length == 0) {
      return new HashSet<>();
    }
    if (args.length == 1) {
      return (Set<Object>) args[0];
    }
    Set result = new HashSet((Set)args[0]);
    for (int i = 1, argsLength = args.length; i < argsLength; i++) {
      result.addAll((Set)args[i]);
    }
    return result;
  }
}
