package core.procedures.delayed;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.IDeref;

import java.util.concurrent.ExecutionException;

public final class Deref extends AFn {

  public Deref() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{IDeref.class}));
  }

  @Override
  public String getName() {
    return "deref";
  }

  @Override
  public Object apply1(Object arg) {
    try {
      return ((IDeref)arg).deref();
    } catch (ExecutionException | InterruptedException e) {
      throw new RuntimeException(e);
    }
  }
}
