package core.scm;

import core.writer.Writer;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

public class SCMPromise extends CompletableFuture<Object> implements IDeref, ISCMClass {

  @Override
  public Object deref() {
    try {
      return get();
    } catch (ExecutionException | InterruptedException e) {
      throw new RuntimeException(e);
    }
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.PROMISE;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder("#<").append("promise");
    if (isCompletedExceptionally()) {
      sb.append("!error!");
    } else if (isDone()) {
      try {
        sb.append("!").append(Writer.write(get()));
      } catch (InterruptedException | ExecutionException e) {
        e.printStackTrace();
      }
    } else if (isCancelled()) {
      sb.append(":cancelled");
    } else {
      sb.append(":pending");
    }
    return sb.append(">").toString();
  }
}
