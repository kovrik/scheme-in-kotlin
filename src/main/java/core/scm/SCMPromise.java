package core.scm;

import core.writer.Writer;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

public class SCMPromise extends CompletableFuture<Object> implements IDeref, ISCMClass {

  @Override
  public Object deref() {
    return getValue();
  }

  private Object getValue() {
    if (isDone()) {
      try {
        return get();
      } catch (InterruptedException | ExecutionException e) {
        if (e.getCause() instanceof RuntimeException) {
          throw (RuntimeException)e.getCause();
        }
        throw new RuntimeException(e.getMessage());
      }
    }
    return null;
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
      Object value = getValue();
      sb.append("!").append(value == this ? "(this promise)" : Writer.write(value));
    } else if (isCancelled()) {
      sb.append(":cancelled");
    } else {
      sb.append(":pending");
    }
    return sb.append(">").toString();
  }
}
