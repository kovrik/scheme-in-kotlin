package main.core.ast;

import java.util.HashMap;
import java.util.Map;

public class SCMBoolean {

  public static final SCMSymbol TRUE  = new SCMSymbol("#t");
  public static final SCMSymbol FALSE = new SCMSymbol("#f");

  private static final Map<Object, Boolean> VALUES = new HashMap<Object, Boolean>();
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
}
