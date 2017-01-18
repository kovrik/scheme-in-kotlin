package core.scm;

public class SCMBoolean {

  /**
   * Converts any Object to Boolean.
   * Returns FALSE only if value is FALSE itself or null.
   * Returns TRUE otherwise.
   */
  public static Boolean toBoolean(Object value) {
    if (value instanceof Boolean) {
      return (Boolean)value;
    }
    return value != null;
  }
}
