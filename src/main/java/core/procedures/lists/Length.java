package core.procedures.lists;

import core.procedures.AFn;
import core.scm.FnArgs;

import java.util.List;

@FnArgs(args = {List.class})
public class Length extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "length";
  }

  @Override
  public Long invoke(Object... args) {
    return (long)((List)args[0]).size();
  }
}
