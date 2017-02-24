package core.procedures.lists;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMClass;

import java.util.List;

public final class Length extends AFn {

  public Length() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{SCMClass.SCMProperList.class}));
  }

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
