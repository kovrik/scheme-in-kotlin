package core.scm;

public class SCMSymbol implements ISCMClass {

  private static final String SPECIAL_CHARS = "()[]{}\",'`;|\\";

  private final String value;
  private boolean escape = false;

  public SCMSymbol(String value) {
    this.value = value;
  }

  public SCMSymbol(String value, boolean escape) {
    this.value = value;
    this.escape = escape;
  }

  public boolean isEscape() {
    return escape;
  }

  public void setEscape(boolean escape) {
    this.escape = escape;
  }

  /* Check if Symbol has Special Characters and needs to be escaped */
  public boolean hasSpecialChars() {
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
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || !(o instanceof SCMSymbol)) return false;
    SCMSymbol scmSymbol = (SCMSymbol) o;
    return value != null ? value.equals(scmSymbol.value) : scmSymbol.value == null;
  }

  @Override
  public int hashCode() {
    return value != null ? value.hashCode() : 0;
  }

  @Override
  public String toString() {
    return value;
  }
}
