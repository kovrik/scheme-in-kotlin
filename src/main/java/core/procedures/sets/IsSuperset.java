package core.procedures.sets;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.Set;

public final class IsSuperset extends AFn {

  public IsSuperset() {
    super(new FnArgsBuilder().min(2).max(2).mandatory(new Class[] {Set.class, Set.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "superset?";
  }

  @Override
  public Boolean apply2(Object set1, Object set2) {
    return ((Set)set1).containsAll((Set)set2);
  }
}
