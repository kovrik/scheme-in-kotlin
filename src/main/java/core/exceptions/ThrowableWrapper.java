package core.exceptions;

public class ThrowableWrapper extends RuntimeException {

  private final Throwable throwable;

  public ThrowableWrapper(Throwable throwable) {
    super(throwable);
    this.throwable = throwable;
  }

  public Throwable get() {
    return throwable;
  }

  @Override
  public synchronized Throwable fillInStackTrace() {
    return null;
  }
}
