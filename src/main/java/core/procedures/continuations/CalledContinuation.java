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

  /* Do not fill in the execution stack trace (we don't need it anyway) to make CalledContinuations much faster */
  @Override
  public synchronized Throwable fillInStackTrace() {
    return null;
  }

  @Override
  public String toString() {
    return "CalledContinuation{value=" + value + ", continuation=" + continuation + '}';
  }
}
