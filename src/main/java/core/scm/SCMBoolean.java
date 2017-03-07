package core.scm;

public class SCMBoolean {

  /**
   * Converts any Object to boolean.
   * Returns FALSE only if value is FALSE itself or null.
   * Returns TRUE otherwise.
   */
  public static boolean toBoolean(Object value) {
    return (value instanceof Boolean) ? (boolean)value : value != null;
  }
}
