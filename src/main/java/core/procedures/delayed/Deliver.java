package core.procedures.delayed;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.concurrent.CompletableFuture;

public final class Deliver extends AFn {

  public Deliver() {
    super(new FnArgsBuilder().min(2).max(2).mandatory(new Class[]{CompletableFuture.class, Object.class}).build());
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
