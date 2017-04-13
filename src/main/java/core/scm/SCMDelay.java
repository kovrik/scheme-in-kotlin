package core.scm;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.ReentrantDelayException;
import core.writer.Writer;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.atomic.AtomicBoolean;

public class SCMDelay extends CompletableFuture<Object> implements ISCMClass, IDeref {

  private final AtomicBoolean forced = new AtomicBoolean(false);

  private final Evaluator evaluator;
  private final Environment env;
  private final Object expr;

  public SCMDelay(Object expr, Environment env, Evaluator evaluator) {
    super();
    this.evaluator = evaluator;
    this.env = env;
    this.expr = expr;
  }

  @Override
  public Object deref() {
    if (isCancelled()) {
      return null;
    }
    if (isCompletedExceptionally() || isDone()) {
      return getValue();
    }
    if (!forced.compareAndSet(false, true)) {
      /* Do not allow delay to be forced twice */
      throw new ReentrantDelayException(this);
    }
    try {
      /* Always run delay in current thread */
      complete(evaluator.eval(expr, env));
      return get();
    } catch (Exception e) {
      completeExceptionally(e);
    }
    return getValue();
  }

  private Object getValue() {
    try {
      return get();
    } catch (InterruptedException | ExecutionException e) {
      if (e.getCause() instanceof RuntimeException) {
        throw (RuntimeException) e.getCause();
      }
      throw new RuntimeException(e.getMessage());
    }
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.DELAY;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder("#<").append("delay");
    if (isCompletedExceptionally()) {
      Object value;
      try {
        value = getValue();
      } catch (RuntimeException e) {
        value = e;
      }
      sb.append("!error!").append(value == this ? "(this delay)" : Writer.write(value));
    } else if (isDone()) {
      Object value = getValue();
      sb.append("!").append(value == this ? Writer.write("(this delay)") : Writer.write(value));
    } else if (isCancelled()) {
      sb.append(":cancelled");
    } else if (forced.get()) {
      sb.append(":running");
    } else {
      sb.append(":pending");
    }
    return sb.append(">").toString();
  }
}
