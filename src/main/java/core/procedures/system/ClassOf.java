package core.procedures.system;

import core.procedures.AFn;
import core.scm.FnArgs;
import core.scm.SCMClass;

@FnArgs(args = {Object.class})
public class ClassOf extends AFn {

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return "class-of";
  }

  @Override
  public Object invoke(Object... args) {
    return SCMClass.scmClassOf(args[0]);
  }
}
