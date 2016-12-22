package core.procedures.cons;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMClass;
import core.scm.SCMCons;

import java.util.List;

@FnArgs(minArgs = 1, maxArgs = 1, mandatoryArgsTypes = {SCMClass.SCMProperList.class})
public class Reverse extends AFn {

  @Override
  public String getName() {
    return "reverse";
  }

  @Override
  public Object apply(Object... args) {
    SCMCons<Object> result = SCMCons.list();
    for (Object o : (List)args[0]) {
      result.push(o);
    }
    return result;
  }
}
