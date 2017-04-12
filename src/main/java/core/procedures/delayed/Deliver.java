package core.procedures.delayed;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.concurrent.CompletableFuture;

public final class Deliver extends AFn {

  public Deliver() {
    super(new FnArgsBuilder().minArgs(2).maxArgs(2).mandatoryArgsTypes(new Class[]{CompletableFuture.class, Object.class}));
  }

  @Override
  public String getName() {
    return "deliver";
  }

  @Override
  public CompletableFuture<Object> apply2(Object promise, Object value) {
    CompletableFuture<Object> p = (CompletableFuture)promise;
    if (p.isDone() || p.isCompletedExceptionally()) {
      return null;
    }
    p.complete(value);
    return p;
  }
}
