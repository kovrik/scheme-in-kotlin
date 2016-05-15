package core.scm;

import java.util.HashMap;
import java.util.Map;

public class SCMBoolean extends SCMSymbol {

  public static final SCMBoolean TRUE  = new SCMBoolean("#t");
  public static final SCMBoolean FALSE = new SCMBoolean("#f");

  private static final Map<Object, Boolean> VALUES = new HashMap<Object, Boolean>();

  public SCMBoolean(String value) {
    super("");
    if ("#t".equals(value)) {
      this.value = value;
    } else if ("#f".equals(value)) {
      this.value = value;
    } else {
      throw new IllegalArgumentException("Unknown boolean: " + value);
    }
  }

  static {
    VALUES.put(Boolean.TRUE, true);
    VALUES.put(TRUE, true);
    VALUES.put(Boolean.FALSE, false);
    VALUES.put(FALSE, false);
  }

  public static boolean valueOf(Object value) {
    Boolean result = VALUES.get(value);
    if (result == null) {
      return true;
    }
    return result;
  }

  public static SCMBoolean toSCMBoolean(Boolean value) {
    if (value) {
      return TRUE;
    }
    return FALSE;
  }
}
