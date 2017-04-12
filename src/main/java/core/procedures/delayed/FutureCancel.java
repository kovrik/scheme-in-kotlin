package core.procedures.delayed;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.concurrent.Future;

public final class FutureCancel extends AFn {

  public FutureCancel() {
    super(new FnArgsBuilder().minArgs(1).maxArgs(1).mandatoryArgsTypes(new Class[]{Future.class}));
  }

  @Override
  public String getName() {
    return "future-cancel";
  }

  @Override
  public Boolean apply1(Object arg) {
    return ((Future)arg).cancel(true);
  }
}
