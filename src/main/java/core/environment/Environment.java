package core.environment;

import core.exceptions.UndefinedIdentifierException;

import java.util.*;

public class Environment extends HashMap<Object, Object> {

  /* Value for undefined identifiers. Required to distinguish undefined and nil bindings */
  public static final Object UNDEFINED = new Object();

  private final Environment outer;

  public Environment(int size, Environment outer) {
    super(size);
    this.outer = outer;
  }

  public Environment(Environment outer) {
    this.outer = outer;
  }

  public Object findOrDefault(Object key, Object defaultValue) {
    Environment current = this;
    while (true) {
      if (current.containsKey(key)) return current.get(key);
      if (current.outer == null)    return defaultValue;
      current = current.outer;
    }
  }

  public Object findAndPut(Object key, Object value) {
    Environment current = this;
    while (true) {
      if (current.containsKey(key)) return current.put(key, value);
      if (current.outer == null)    throw new UndefinedIdentifierException(key.toString());
      current = current.outer;
    }
  }

  @Override
  public String toString() {
    return "#<environment:" + (outer == null ? "root" : super.toString()) + ">";
  }
}
