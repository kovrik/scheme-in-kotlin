package core.procedures.system;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.Void;

public final class Pst extends AFn {

  public Pst() {
    super(new FnArgsBuilder().min(1).max(1).mandatory(new Class[]{Throwable.class}).build());
  }

  @Override
  public String getName() {
    return "pst";
  }

  @Override
  public Object apply1(Object arg) {
    ((Throwable)arg).printStackTrace();
    return Void.VOID;
  }
}
