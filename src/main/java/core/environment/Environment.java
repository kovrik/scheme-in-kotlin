package core.environment;

import core.exceptions.UndefinedIdentifierException;
import core.scm.Type;

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
    if (!containsKey(key)) {
      if (outer == null) {
        return defaultValue;
      }
      return outer.findOrDefault(key, defaultValue);
    }
    return get(key);
  }

  public Object findAndPut(Object key, Object value) {
    if (!containsKey(key)) {
      if (outer == null) {
        throw new UndefinedIdentifierException(key.toString());
      }
      return outer.findAndPut(key, value);
    }
    return put(key, value);
  }

  @Override
  public String toString() {
    return "#<environment:" + ((outer == null) ? "GLOBAL" : super.toString()) + ">";
  }
}
