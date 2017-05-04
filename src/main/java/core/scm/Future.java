package core.scm;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.writer.Writer;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.FutureTask;

public class Future extends FutureTask implements ITyped, IDeref {

  public Future(Object expr, Environment env, Evaluator evaluator) {
    super(() -> evaluator.eval(expr, env));
  }

  @Override
  public Object deref() {
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
  public Type getType() {
    return Type.FUTURE;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder("#<").append("future");
    if (isDone()) {
      sb.append("!");
      Object value;
      try {
        value = deref();
      } catch (RuntimeException e) {
        sb.append("error!");
        value = e;
      }
      sb.append(value == this ? "(this future)" : Writer.write(value));
    } else if (isCancelled()) {
      sb.append(":cancelled");
    } else {
      sb.append(":pending");
    }
    return sb.append(">").toString();
  }
}
