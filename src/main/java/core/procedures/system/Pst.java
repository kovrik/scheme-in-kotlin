package core.procedures.system;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.SCMVoid;

public final class Pst extends AFn {

  public Pst() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{Throwable.class}));
  }

  @Override
  public String getName() {
    return "pst";
  }

  @Override
  public Object apply1(Object arg) {
    ((Throwable)arg).printStackTrace();
    return SCMVoid.VOID;
  }
}
