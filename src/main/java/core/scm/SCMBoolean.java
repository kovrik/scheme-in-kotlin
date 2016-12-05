package core.scm;

import java.util.HashMap;
import java.util.Map;

public class SCMBoolean implements ISCMClass {

  public static final SCMBoolean TRUE  = new SCMBoolean("#t");
  public static final SCMBoolean FALSE = new SCMBoolean("#f");

  private static final Map<Object, Boolean> VALUES = new HashMap<>();
  static {
    VALUES.put(Boolean.TRUE, true);
    VALUES.put(TRUE, true);
    VALUES.put(Boolean.FALSE, false);
    VALUES.put(FALSE, false);
  }

  private final String value;

  private SCMBoolean(String value) {
    this.value = value;
    if (!"#t".equals(value) && !"#f".equals(value)) {
      throw new IllegalArgumentException("Unknown boolean: " + value);
    }
  }

  public boolean toBoolean() {
    return valueOf(this);
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.BOOLEAN;
  }

  @Override
  public String toString() {
    return value;
  }

  public static boolean valueOf(Object value) {
    Boolean result = VALUES.get(value);
    return (result == null) ? true : result;
  }

  public static SCMBoolean toSCMBoolean(Boolean value) {
    return value ? TRUE : FALSE;
  }
}
