package core.scm;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.utils.InternPool;

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
public class SCMSymbol extends AFn implements ISCMClass, INamed, IMeta {

  /* Pool of all interned symbols */
  private static final InternPool<SCMSymbol> POOL = new InternPool<>();

  private static final String SPECIAL_CHARS = "()[]{}\",'`;|\\";

  private final String name;
  private final boolean escape;

  /* Metadata */
  private Map meta;

  public static SCMSymbol intern(String name) {
    // always intern symbols
    return POOL.intern(new SCMSymbol(name));
  }

  private SCMSymbol(String name) {
    this(name, null);
  }

  /* Create Symbol with Metadata attached.
   * Note that symbols with metadata are not interned!
   * TODO check if should actually intern syms with meta?
   */
  public SCMSymbol(String name, Map meta) {
    this.name = name;
    this.escape = hasSpecialChars();
    this.meta = meta;
  }

  @Override
  public Map meta() {
    return meta;
  }

  public boolean isEscape() {
    return escape;
  }

  /* Check if Symbol has Special Characters and needs to be escaped */
  private boolean hasSpecialChars() {
    /* Check if string representation must be escaped */
    if (name.isEmpty() || Character.isDigit(name.charAt(0))) {
      return true;
    }
    if (name.charAt(0) == '#') {
      if (name.length() == 1) {
        return true;
      } else if (name.charAt(1) != '%') {
        return true;
      }
    }
    if (name.length() == 1 && name.charAt(0) == '.') {
      return true;
    }
    for (char c : name.toCharArray()) {
      if (Character.isWhitespace(c) || SPECIAL_CHARS.indexOf(c) > -1) {
        return true;
      }
    }
    return false;
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public Object apply(Object... args) {
    if (args.length == 0 || args.length > 2) {
      throw new ArityException(toString() + " Symbol", 1, 2, args.length);
    }
    Object defaultValue = (args.length == 2) ? args[1] : null;
    return ((Map)args[0]).getOrDefault(this, defaultValue);
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.SYMBOL;
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || !(o instanceof SCMSymbol)) return false;
    SCMSymbol scmSymbol = (SCMSymbol) o;
    return name != null ? name.equals(scmSymbol.name) : scmSymbol.name == null;
  }

  @Override
  public int hashCode() {
    return name.hashCode() + 1037096266;
  }

  @Override
  public String toString() {
    return name;
  }
}
