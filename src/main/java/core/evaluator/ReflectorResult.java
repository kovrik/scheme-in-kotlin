package core.evaluator;

/* Wrapper to avoid downcast */
@Deprecated
public final class ReflectorResult {

  private final Object value;

  public ReflectorResult(Object value) {
    this.value = value;
  }

  public Object get() {
    return value;
  }

  public static Object maybeWrap(Object value) {
    if (value instanceof Number) {
      return new ReflectorResult(value);
    }
    return value;
  }
}

