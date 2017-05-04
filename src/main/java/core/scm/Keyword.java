package core.scm;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;
import core.utils.InternPool;

import java.util.Map;

public class Keyword extends AFn implements ITyped, INamed {

  /* Pool of all interned keywords */
  private static final InternPool<Keyword> POOL = new InternPool<>();

  private final String name;

  private Keyword(String name) {
    super(new FnArgsBuilder().mandatory(new Class[]{Map.class}).build());
    this.name = name;
  }

  public static Keyword intern(String value) {
    // always intern keywords
    return POOL.intern(new Keyword(value));
  }

  @Override
  public Type getType() {
    return Type.KEYWORD;
  }

  @Override
  public String toString() {
    return ':' + name;
  }

  @Override
  public boolean isPure() {
    return true;
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public Object apply(Object... args) {
    if (args.length == 0 || args.length > 2) {
      throw new ArityException(toString() + " Keyword", 1, 2, args.length);
    }
    Object defaultValue = (args.length == 2) ? args[1] : null;
    return ((Map)args[0]).getOrDefault(this, defaultValue);
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    Keyword that = (Keyword) o;
    return name != null ? name.equals(that.name) : that.name == null;
  }

  @Override
  public int hashCode() {
    return name != null ? name.hashCode() + 1077096266 : 1077096266;
  }
}
