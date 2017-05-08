package core.procedures.delayed;

import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.scm.Promise;

import java.util.concurrent.CompletableFuture;

public final class PromiseProc extends AFn {

  public PromiseProc() {
    super(new FnArgsBuilder().max(0).build());
  }

  @Override
  public String getName() {
    return "promise";
  }

  @Override
  public CompletableFuture<Object> apply0() {
    return new Promise();
  }
}
