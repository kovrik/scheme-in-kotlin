package core.scm;

import core.exceptions.ArityException;
import core.procedures.AFn;
import core.procedures.FnArgsBuilder;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class SCMKeyword extends AFn implements ISCMClass, INamed {

  private static final ConcurrentHashMap<String, SCMKeyword> CACHE = new ConcurrentHashMap<>();

  private final String name;

  private SCMKeyword(String name) {
    super(new FnArgsBuilder().mandatoryArgsTypes(new Class[]{Map.class}));
    // always intern keywords
    this.name = name.intern();
  }

  public static SCMKeyword of(String value) {
    return CACHE.computeIfAbsent(value, k -> new SCMKeyword(value));
  }

  @Override
  public SCMClass getSCMClass() {
    return SCMClass.KEYWORD;
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
      throw new ArityException(toString() + " keyword", 1, 2, args.length);
    }
    Object defaultValue = (args.length == 2) ? args[1] : null;
    return ((Map)args[0]).getOrDefault(this, defaultValue);
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    SCMKeyword that = (SCMKeyword) o;
    return name != null ? name.equals(that.name) : that.name == null;
  }

  @Override
  public int hashCode() {
    return name != null ? name.hashCode() + 1077096266 : 1077096266;
  }
}
