package core.procedures.lists;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMClass;

import java.util.List;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {SCMClass.SCMProperList.class})
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
  public Long apply1(Object arg) {
    return (long)((List)arg).size();
  }
}
