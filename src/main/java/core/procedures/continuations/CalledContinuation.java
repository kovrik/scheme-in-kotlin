package core.procedures.continuations;

public class CalledContinuation extends RuntimeException {

  private final Object value;
  private final Continuation continuation;

  CalledContinuation(Object value, Continuation cont) {
    this.value = value;
    this.continuation = cont;
  }

  public Object getValue() {
    return value;
  }

  public Continuation getContinuation() {
    return continuation;
  }
}
