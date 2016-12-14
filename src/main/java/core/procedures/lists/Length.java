package core.procedures.lists;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMClass;

import java.util.List;

@FnArgs(args = {SCMClass.SCMProperList.class})
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
  public Long apply(Object... args) {
    return (long)((List)args[0]).size();
  }
}
