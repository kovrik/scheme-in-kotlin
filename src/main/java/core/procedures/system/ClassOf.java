package core.procedures.system;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMClass;

public class ClassOf extends AFn {

  public ClassOf() {
    super(new FnArgsBuilder().min(1).max(1).build());
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "class-of";
  }

  @Override
  public Object apply1(Object arg) {
    SCMClass scmClass = SCMClass.classOf(arg);
    if (scmClass == null) {
      return arg.getClass();
    }
    return scmClass;
  }
}
