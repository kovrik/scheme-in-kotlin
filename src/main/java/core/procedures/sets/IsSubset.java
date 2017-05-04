package core.procedures.sets;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.Set;

public final class IsSubset extends AFn {

  public IsSubset() {
    super(new FnArgsBuilder().min(2).max(2).mandatory(new Class[] {Set.class, Set.class}).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "subset?";
  }

  @Override
  public Boolean apply2(Object set1, Object set2) {
    return ((Set)set2).containsAll((Set)set1);
  }
}
