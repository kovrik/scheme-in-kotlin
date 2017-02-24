package core.procedures.cons;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMClass;
import core.scm.SCMCons;

import java.util.List;

public final class Reverse extends AFn {

  public Reverse() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1)
                             .mandatoryArgsTypes(new Class[]{SCMClass.SCMProperList.class}));
  }

  @Override
  public String getName() {
    return "reverse";
  }

  @Override
  public Object apply1(Object arg) {
    SCMCons<Object> result = SCMCons.list();
    ((List)arg).forEach(result::push);
    return result;
  }
}
