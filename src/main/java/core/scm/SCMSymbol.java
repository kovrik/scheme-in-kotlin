package core.scm;

import java.util.HashMap;
import java.util.Map;

/* Symbol class
 *
 * By default all symbols are interned and stored in INTERNED Map.
 *
 * This means that two values:
 *
 *   (define s1 'test)
 *   (define s2 'test)
 *
 * will reference to the same symbol object.
 */
public class SCMSymbol implements ISCMClass, INamed {

  /* Map of all interned symbols */
  private static final Map<String, SCMSymbol> INTERNED = new HashMap<>();

  private static final String SPECIAL_CHARS = "()[]{}\",'`;|\\";

  private final String value;
  private boolean escape = false;

  public static SCMSymbol of(String value) {
    return INTERNED.computeIfAbsent(value, k -> new SCMSymbol(value));
  }

  private SCMSymbol(String value) {
    // always intern symbols
    this.value = value.intern();
    this.escape = hasSpecialChars();
  }

  public boolean isEscape() {
    return escape;
  }

  /* Check if Symbol has Special Characters and needs to be escaped */
  private boolean hasSpecialChars() {
    /* Check if string representation must be escaped */
    if (value.isEmpty() || Character.isDigit(value.charAt(0))) {
      return true;
    }
    if (value.charAt(0) == '#') {
      if (value.length() == 1) {
        return true;
      } else if (value.charAt(1) != '%') {
        return true;
      }
    }
    if (value.length() == 1 && value.charAt(0) == '.') {
      return true;
    }
    for (char c : value.toCharArray()) {
      if (Character.isWhitespace(c) || SPECIAL_CHARS.indexOf(c) > -1) {
        return true;
      }
    }
    return false;
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.SYMBOL;
  }

  @Override
  public String getName() {
    return value;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || !(o instanceof SCMSymbol)) return false;
    SCMSymbol scmSymbol = (SCMSymbol) o;
    return value != null ? value.equals(scmSymbol.value) : scmSymbol.value == null;
  }

  @Override
  public int hashCode() {
    return value != null ? value.hashCode() + 1037096266 : 1037096266;
  }

  @Override
  public String toString() {
    return value;
  }
}
