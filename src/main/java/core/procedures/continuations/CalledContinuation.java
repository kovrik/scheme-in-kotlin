package core.procedures.continuations;

public class CalledContinuation extends RuntimeException {

  private final Object value;
  private final Continuation continuation;

  CalledContinuation(Object value, Continuation cont) {
    super("CalledContinuationException");
    this.value = value;
    this.continuation = cont;
  }

  public Object getValue() {
    return value;
  }

  public Continuation getContinuation() {
    return continuation;
  }

  @Override
  public String toString() {
    return "CalledContinuation{" + "value=" + value + ", continuation=" + continuation + '}';
  }
}
