package core.procedures.delayed;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.concurrent.CompletableFuture;

public final class Promise extends AFn {

  public Promise() {
    super(new FnArgsBuilder().max(0).build());
  }

  @Override
  public String getName() {
    return "promise";
  }

  @Override
  public CompletableFuture<Object> apply0() {
    return new core.scm.Promise();
  }
}
