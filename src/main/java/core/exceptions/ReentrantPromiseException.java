package core.exceptions;

import core.procedures.delayed.SCMPromise;
import core.writer.Writer;

public class ReentrantPromiseException extends IllegalArgumentException {

  public ReentrantPromiseException(SCMPromise promise) {
    super(String.format("Reentrant promise: %s", Writer.write(promise)), null);
  }
}
